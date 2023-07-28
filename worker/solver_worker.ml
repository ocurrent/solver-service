(* This could is largely borrowed from the OCluster worker. Along
   with some parts of the solver-worker library. See the LICENSE file for the
   full license. *)

open Eio.Std
open Lwt.Infix
open Capnp_rpc_lwt

let min_reconnect_time = 10.0
(* Don't try to connect more than once per 10 seconds *)

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "worker"

  let jobs_accepted =
    let help = "Number of jobs accepted in total" in
    Counter.v ~help ~namespace ~subsystem "jobs_accepted_total"

  let job_time =
    let help = "Time jobs ran for" in
    Summary.v_label ~label_name:"result" ~help ~namespace ~subsystem
      "job_time_seconds"

  let running_jobs =
    let help = "Number of jobs currently running" in
    Gauge.v ~help ~namespace ~subsystem "running_jobs"
end

type t = {
  solver : Solver_service.Solver.t;
  name : string;
  registration_service : Cluster_api.Raw.Client.Registration.t Sturdy_ref.t;
  capacity : int;
  mutable in_use : int; (* Number of active builds *)
  cond : unit Lwt_condition.t;
  (* Fires when a build finishes (or switch turned off) *)
}

let metrics = function
  | `Agent ->
    let open Lwt.Infix in
    let content_type = "text/plain; version=0.0.4; charset=utf-8" in
    Prometheus.CollectorRegistry.(collect default) >>= fun data ->
    Lwt_result.return
      ( content_type,
        Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data )
  | `Host ->
    failwith "No host metrics from solver service"

let build ~cancelled ~log t descr =
  let module R = Cluster_api.Raw.Reader.JobDescr in
  match Cluster_api.Submission.get_action descr with
  | Custom_build c ->
    Log.info (fun f ->
        f "Got request to build a job of kind \"%s\""
          (Cluster_api.Custom.kind c));
    (* Oddly, the protocol has us report cancellation and errors as "successful" jobs with the error inside! *)
    let output = Custom.solve ~cancelled ~solver:t.solver ~log c in
    Log_data.write log "Job succeeded\n";
    (Ok output, "ok")
  | _ ->
    let msg = "Only custom builds are supported" in
    (Error (`Msg msg), "build failed")

let loop t queue =
  let rec loop () =
    if t.in_use >= t.capacity then (
      Log.info (fun f ->
          f "At capacity. Waiting for a build to finish before requesting \
             more...");
      Lwt_condition.wait t.cond >>= loop)
    else
      let outcome, set_outcome = Lwt.wait () in
      let log = Log_data.create () in
      Log.info (fun f -> f "Requesting a new job...");
      let switch = Lwt_switch.create () in
      let cancelled, set_cancelled = Promise.create () in
      Lwt_switch.add_hook (Some switch) (fun () -> Promise.resolve set_cancelled (); Lwt.return_unit);
      let pop =
        Capability.with_ref
          (Cluster_api.Job.local ~switch ~outcome
             ~stream_log_data:(Log_data.stream log))
        @@ fun job -> Cluster_api.Queue.pop queue job
      in
      pop >>= fun request ->
      t.in_use <- t.in_use + 1;
      Prometheus.Gauge.set Metrics.running_jobs (float_of_int t.in_use);
      Prometheus.Counter.inc_one Metrics.jobs_accepted;
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
               Lwt_eio.run_eio @@ fun () ->
               Log_data.info log "Building on %s" t.name;
               let t0 = Unix.gettimeofday () in
               match build ~cancelled ~log t request with
               | (outcome, metric_label) ->
                 let t1 = Unix.gettimeofday () in
                 Prometheus.Summary.observe
                   (Metrics.job_time metric_label)
                   (t1 -. t0);
                 Log_data.close log;
                 Lwt.wakeup set_outcome outcome
               | exception ex ->
                 let bt = Printexc.get_raw_backtrace () in
                 let t1 = Unix.gettimeofday () in
                 Prometheus.Summary.observe (Metrics.job_time "error")
                   (t1 -. t0);
                 Log.warn (fun f -> f "Build failed: %a" Fmt.exn_backtrace (ex, bt));
                 Log_data.write log
                   (Fmt.str "Uncaught exception: %a@." Fmt.exn ex);
                 Log_data.close log;
                 Lwt.wakeup_exn set_outcome ex
            )
            (fun () ->
               t.in_use <- t.in_use - 1;
               Prometheus.Gauge.set Metrics.running_jobs
                 (float_of_int t.in_use);
               Lwt_switch.turn_off switch >>= fun () ->
               Lwt_condition.broadcast t.cond ();
               Lwt.return_unit));
      loop ()
  in
  loop ()

let self_update () = failwith "TODO: Self-update"

let run ~name ~capacity solver registration_service =
  Lwt_eio.run_lwt @@ fun () ->
  let t = {
    solver;
    name;
    registration_service;
    cond = Lwt_condition.create ();
    capacity;
    in_use = 0;
  } in
  let rec reconnect () =
    let connect_time = Unix.gettimeofday () in
    Lwt.catch
      (fun () ->
         Sturdy_ref.connect_exn t.registration_service >>= fun reg ->
         Capability.with_ref reg @@ fun reg ->
         let queue =
           let api = Cluster_api.Worker.local ~metrics ~self_update () in
           let queue = Cluster_api.Registration.register reg ~name ~capacity api in
           Capability.dec_ref api;
           queue
         in
         Capability.with_ref queue @@ fun queue ->
         Lwt.catch
           (fun () -> loop t queue)
           (fun ex ->
              Lwt.pause () >>= fun () ->
              match Capability.problem queue with
              | Some problem ->
                Log.info (fun f -> f "Worker loop failed (probably because queue connection failed): %a" Fmt.exn ex);
                Lwt.fail (Failure (Fmt.to_to_string Capnp_rpc.Exception.pp problem))    (* Will retry *)
              | None ->
                raise ex
           )
      )
      (fun ex ->
         let delay = max 0.0 (connect_time +. min_reconnect_time -. Unix.gettimeofday ()) in
         Log.info (fun f -> f "Lost connection to scheduler (%a). Will retry in %.1fs…" Fmt.exn ex delay);
         Lwt_unix.sleep delay >>= reconnect
      )
  in
  reconnect ()
