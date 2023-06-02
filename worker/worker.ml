(* This could is largely borrowed from the OCluster worker. Along
   with some parts of the solver-worker library. See the end of the
   file for the full license. *)

open Lwt.Infix
open Solver_worker
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

  let docker_push_time =
    let help = "Time uploading to Docker Hub" in
    Summary.v ~help ~namespace ~subsystem "docker_push_time_seconds"

  let docker_prune_time =
    let help = "Time spent pruning Docker cache" in
    Summary.v ~help ~namespace ~subsystem "docker_prune_time_seconds"

  let running_jobs =
    let help = "Number of jobs currently running" in
    Gauge.v ~help ~namespace ~subsystem "running_jobs"

  let healthcheck_time =
    let help = "Time to perform last healthcheck" in
    Gauge.v ~help ~namespace ~subsystem "healthcheck_time_seconds"

  let unhealthy =
    let help = "Number of unhealthy workers" in
    Gauge.v ~help ~namespace ~subsystem "unhealthy"
end

type build =
  switch:Lwt_switch.t ->
  log:Solver_worker.Log_data.t ->
  src:string ->
  secrets:(string * string) list ->
  Cluster_api.Custom.payload Cluster_api.Custom.t ->
  (string, [ `Cancelled | `Msg of string ]) Lwt_result.t

type t = {
  name : string;
  context : Solver_worker.Context.t;
  build : build;
  registration_service : Cluster_api.Raw.Client.Registration.t Sturdy_ref.t;
  capacity : int;
  mutable in_use : int; (* Number of active builds *)
  cond : unit Lwt_condition.t;
  (* Fires when a build finishes (or switch turned off) *)
  mutable cancel : unit -> unit; (* Called if switch is turned off *)
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
      Lwt.catch
        (fun () ->
          Cohttp_lwt_unix.Client.get
            (Uri.of_string "http://127.0.0.1:9100/metrics")
          >>= fun (resp, body) ->
          match Cohttp.Response.status resp with
          | `OK -> (
              match
                Cohttp.Header.get (Cohttp.Response.headers resp) "content-type"
              with
              | Some content_type ->
                  body |> Cohttp_lwt.Body.to_string >|= fun body ->
                  Ok (content_type, body)
              | None ->
                  Lwt.return
                  @@ Fmt.error_msg
                       "Missing Content-Type in HTTP response from \
                        prometheus-node-exporter")
          | code ->
              Log.warn (fun f ->
                  f "prometheus-node-exporter: %s"
                    (Cohttp.Code.string_of_status code));
              Lwt.return
              @@ Fmt.error_msg "prometheus-node-exporter: %s"
                   (Cohttp.Code.string_of_status code))
        (fun ex ->
          Log.warn (fun f ->
              f "Failed to connect to prometheus-node-exporter: %a" Fmt.exn ex);
          Lwt.return
          @@ Fmt.error_msg "Failed to connect to prometheus-node-exporter")

let build ~switch ~log t descr =
  let module R = Cluster_api.Raw.Reader.JobDescr in
  (* let cache_hint = R.cache_hint_get descr in *)
  let secrets =
    R.secrets_get_list descr
    |> List.map (fun t -> Cluster_api.Raw.Reader.Secret.(id_get t, value_get t))
  in
  (match Cluster_api.Submission.get_action descr with
  | Custom_build c ->
      Log.info (fun f ->
          f "Got request to build a job of kind \"%s\""
            (Cluster_api.Custom.kind c));
      Context.with_build_context t.context ~log descr @@ fun src ->
      t.build ~switch ~log ~src ~secrets c
  | _ -> Lwt.fail (invalid_arg "Only custom builds are supported"))
  >|= function
  | Error `Cancelled ->
      Log_data.write log "Job cancelled\n";
      Log.info (fun f -> f "Job cancelled");
      (Error (`Msg "Build cancelled"), "cancelled")
  | Ok output ->
      Log_data.write log "Job succeeded\n";
      Log.info (fun f -> f "Job succeeded");
      (Ok output, "ok")
  | Error (`Msg msg) ->
      Log.info (fun f -> f "Job failed: %s" msg);
      (Error (`Msg msg), "build failed")

let loop ~switch t queue =
  let rec loop () =
    match switch with
    | Some switch when not (Lwt_switch.is_on switch) ->
        Log.info (fun f -> f "Builder shutting down (switch turned off)");
        Lwt.return `Cancelled
    | _ ->
        if t.in_use >= t.capacity then (
          Log.info (fun f ->
              f
                "At capacity. Waiting for a build to finish before requesting \
                 more...");
          Lwt_condition.wait t.cond >>= loop)
        else
          let outcome, set_outcome = Lwt.wait () in
          let log = Log_data.create () in
          Log.info (fun f -> f "Requesting a new job...");
          let switch = Lwt_switch.create () in
          let pop =
            Capability.with_ref
              (Cluster_api.Job.local ~switch ~outcome
                 ~stream_log_data:(Log_data.stream log))
            @@ fun job -> Cluster_api.Queue.pop queue job
          in
          t.cancel <- (fun () -> Lwt.cancel pop);
          pop >>= fun request ->
          t.in_use <- t.in_use + 1;
          Prometheus.Gauge.set Metrics.running_jobs (float_of_int t.in_use);
          Prometheus.Counter.inc_one Metrics.jobs_accepted;
          Lwt.async (fun () ->
              Lwt.finalize
                (fun () ->
                  let t0 = Unix.gettimeofday () in
                  Lwt.try_bind
                    (fun () ->
                      Log_data.info log "Building on %s" t.name;
                      build ~switch ~log t request)
                    (fun (outcome, metric_label) ->
                      let t1 = Unix.gettimeofday () in
                      Prometheus.Summary.observe
                        (Metrics.job_time metric_label)
                        (t1 -. t0);
                      Log_data.close log;
                      Lwt.wakeup set_outcome outcome;
                      Lwt.return_unit)
                    (fun ex ->
                      let t1 = Unix.gettimeofday () in
                      Prometheus.Summary.observe (Metrics.job_time "error")
                        (t1 -. t0);
                      Log.warn (fun f -> f "Build failed: %a" Fmt.exn ex);
                      Log_data.write log
                        (Fmt.str "Uncaught exception: %a@." Fmt.exn ex);
                      Log_data.close log;
                      Lwt.wakeup_exn set_outcome ex;
                      Lwt.return_unit))
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

(* Respond to update requests by doing nothing, on the assumption that the
   admin has updated the local package version. *)
let update_normal () = Lwt.return (fun () -> Lwt.return ())

let run ?switch ?prune_threshold ~build ~capacity ~name ~state_dir
    registration_service =
  (match prune_threshold with
  | None ->
      Log.info (fun f ->
          f "Prune threshold not set. Will not check for low disk-space!")
  | Some frac when frac < 0.0 || frac > 100.0 ->
      Fmt.invalid_arg "prune_threshold must be in the range 0 to 100"
  | Some _ -> ());
  let t =
    {
      name;
      context = Context.v ~state_dir;
      registration_service;
      build;
      cond = Lwt_condition.create ();
      capacity;
      in_use = 0;
      cancel = ignore;
    }
  in
  Lwt_switch.add_hook_or_exec switch (fun () ->
      Log.info (fun f -> f "Switch turned off. Will shut down.");
      t.cancel ();
      Lwt_condition.broadcast t.cond ();
      Lwt.return_unit)
  >>= fun () ->
  let rec reconnect () =
    let connect_time = Unix.gettimeofday () in
    Lwt.catch
      (fun () ->
        Sturdy_ref.connect_exn t.registration_service >>= fun reg ->
        Capability.with_ref reg @@ fun reg ->
        let queue =
          let api =
            Cluster_api.Worker.local ~metrics
              ~self_update:(fun () -> Lwt.return_ok ())
              ()
          in
          let queue =
            Cluster_api.Registration.register reg ~name ~capacity api
          in
          Capability.dec_ref api;
          queue
        in
        Capability.with_ref queue @@ fun queue ->
        Lwt.catch
          (fun () -> loop ~switch t queue)
          (fun ex ->
            Lwt.pause () >>= fun () ->
            match (Capability.problem queue, switch) with
            | _, Some switch when not (Lwt_switch.is_on switch) ->
                Lwt.return `Cancelled
            | Some problem, _ ->
                Log.info (fun f ->
                    f
                      "Worker loop failed (probably because queue connection \
                       failed): %a"
                      Fmt.exn ex);
                Lwt.fail
                  (Failure (Fmt.to_to_string Capnp_rpc.Exception.pp problem))
                (* Will retry *)
            | None, _ -> Lwt.return (`Crash ex)))
      (fun ex ->
        let delay =
          max 0.0 (connect_time +. min_reconnect_time -. Unix.gettimeofday ())
        in
        Log.info (fun f ->
            f "Lost connection to scheduler (%a). Will retry in %.1fs..."
              Fmt.exn ex delay);
        Lwt_unix.sleep delay >>= reconnect)
  in
  reconnect () >>= function
  | `Cancelled -> Lwt.return_unit
  | `Crash ex -> Lwt.fail ex

(*
                                  Apache License
                            Version 2.0, January 2004
                         https://www.apache.org/licenses/

    TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

    1. Definitions.

       "License" shall mean the terms and conditions for use, reproduction,
       and distribution as defined by Sections 1 through 9 of this document.

       "Licensor" shall mean the copyright owner or entity authorized by
       the copyright owner that is granting the License.

       "Legal Entity" shall mean the union of the acting entity and all
       other entities that control, are controlled by, or are under common
       control with that entity. For the purposes of this definition,
       "control" means (i) the power, direct or indirect, to cause the
       direction or management of such entity, whether by contract or
       otherwise, or (ii) ownership of fifty percent (50%) or more of the
       outstanding shares, or (iii) beneficial ownership of such entity.

       "You" (or "Your") shall mean an individual or Legal Entity
       exercising permissions granted by this License.

       "Source" form shall mean the preferred form for making modifications,
       including but not limited to software source code, documentation
       source, and configuration files.

       "Object" form shall mean any form resulting from mechanical
       transformation or translation of a Source form, including but
       not limited to compiled object code, generated documentation,
       and conversions to other media types.

       "Work" shall mean the work of authorship, whether in Source or
       Object form, made available under the License, as indicated by a
       copyright notice that is included in or attached to the work
       (an example is provided in the Appendix below).

       "Derivative Works" shall mean any work, whether in Source or Object
       form, that is based on (or derived from) the Work and for which the
       editorial revisions, annotations, elaborations, or other modifications
       represent, as a whole, an original work of authorship. For the purposes
       of this License, Derivative Works shall not include works that remain
       separable from, or merely link (or bind by name) to the interfaces of,
       the Work and Derivative Works thereof.

       "Contribution" shall mean any work of authorship, including
       the original version of the Work and any modifications or additions
       to that Work or Derivative Works thereof, that is intentionally
       submitted to Licensor for inclusion in the Work by the copyright owner
       or by an individual or Legal Entity authorized to submit on behalf of
       the copyright owner. For the purposes of this definition, "submitted"
       means any form of electronic, verbal, or written communication sent
       to the Licensor or its representatives, including but not limited to
       communication on electronic mailing lists, source code control systems,
       and issue tracking systems that are managed by, or on behalf of, the
       Licensor for the purpose of discussing and improving the Work, but
       excluding communication that is conspicuously marked or otherwise
       designated in writing by the copyright owner as "Not a Contribution."

       "Contributor" shall mean Licensor and any individual or Legal Entity
       on behalf of whom a Contribution has been received by Licensor and
       subsequently incorporated within the Work.

    2. Grant of Copyright License. Subject to the terms and conditions of
       this License, each Contributor hereby grants to You a perpetual,
       worldwide, non-exclusive, no-charge, royalty-free, irrevocable
       copyright license to reproduce, prepare Derivative Works of,
       publicly display, publicly perform, sublicense, and distribute the
       Work and such Derivative Works in Source or Object form.

    3. Grant of Patent License. Subject to the terms and conditions of
       this License, each Contributor hereby grants to You a perpetual,
       worldwide, non-exclusive, no-charge, royalty-free, irrevocable
       (except as stated in this section) patent license to make, have made,
       use, offer to sell, sell, import, and otherwise transfer the Work,
       where such license applies only to those patent claims licensable
       by such Contributor that are necessarily infringed by their
       Contribution(s) alone or by combination of their Contribution(s)
       with the Work to which such Contribution(s) was submitted. If You
       institute patent litigation against any entity (including a
       cross-claim or counterclaim in a lawsuit) alleging that the Work
       or a Contribution incorporated within the Work constitutes direct
       or contributory patent infringement, then any patent licenses
       granted to You under this License for that Work shall terminate
       as of the date such litigation is filed.

    4. Redistribution. You may reproduce and distribute copies of the
       Work or Derivative Works thereof in any medium, with or without
       modifications, and in Source or Object form, provided that You
       meet the following conditions:

       (a) You must give any other recipients of the Work or
           Derivative Works a copy of this License; and

       (b) You must cause any modified files to carry prominent notices
           stating that You changed the files; and

       (c) You must retain, in the Source form of any Derivative Works
           that You distribute, all copyright, patent, trademark, and
           attribution notices from the Source form of the Work,
           excluding those notices that do not pertain to any part of
           the Derivative Works; and

       (d) If the Work includes a "NOTICE" text file as part of its
           distribution, then any Derivative Works that You distribute must
           include a readable copy of the attribution notices contained
           within such NOTICE file, excluding those notices that do not
           pertain to any part of the Derivative Works, in at least one
           of the following places: within a NOTICE text file distributed
           as part of the Derivative Works; within the Source form or
           documentation, if provided along with the Derivative Works; or,
           within a display generated by the Derivative Works, if and
           wherever such third-party notices normally appear. The contents
           of the NOTICE file are for informational purposes only and
           do not modify the License. You may add Your own attribution
           notices within Derivative Works that You distribute, alongside
           or as an addendum to the NOTICE text from the Work, provided
           that such additional attribution notices cannot be construed
           as modifying the License.

       You may add Your own copyright statement to Your modifications and
       may provide additional or different license terms and conditions
       for use, reproduction, or distribution of Your modifications, or
       for any such Derivative Works as a whole, provided Your use,
       reproduction, and distribution of the Work otherwise complies with
       the conditions stated in this License.

    5. Submission of Contributions. Unless You explicitly state otherwise,
       any Contribution intentionally submitted for inclusion in the Work
       by You to the Licensor shall be under the terms and conditions of
       this License, without any additional terms or conditions.
       Notwithstanding the above, nothing herein shall supersede or modify
       the terms of any separate license agreement you may have executed
       with Licensor regarding such Contributions.

    6. Trademarks. This License does not grant permission to use the trade
       names, trademarks, service marks, or product names of the Licensor,
       except as required for reasonable and customary use in describing the
       origin of the Work and reproducing the content of the NOTICE file.

    7. Disclaimer of Warranty. Unless required by applicable law or
       agreed to in writing, Licensor provides the Work (and each
       Contributor provides its Contributions) on an "AS IS" BASIS,
       WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
       implied, including, without limitation, any warranties or conditions
       of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
       PARTICULAR PURPOSE. You are solely responsible for determining the
       appropriateness of using or redistributing the Work and assume any
       risks associated with Your exercise of permissions under this License.

    8. Limitation of Liability. In no event and under no legal theory,
       whether in tort (including negligence), contract, or otherwise,
       unless required by applicable law (such as deliberate and grossly
       negligent acts) or agreed to in writing, shall any Contributor be
       liable to You for damages, including any direct, indirect, special,
       incidental, or consequential damages of any character arising as a
       result of this License or out of the use or inability to use the
       Work (including but not limited to damages for loss of goodwill,
       work stoppage, computer failure or malfunction, or any and all
       other commercial damages or losses), even if such Contributor
       has been advised of the possibility of such damages.

    9. Accepting Warranty or Additional Liability. While redistributing
       the Work or Derivative Works thereof, You may choose to offer,
       and charge a fee for, acceptance of support, warranty, indemnity,
       or other liability obligations and/or rights consistent with this
       License. However, in accepting such obligations, You may act only
       on Your own behalf and on Your sole responsibility, not on behalf
       of any other Contributor, and only if You agree to indemnify,
       defend, and hold each Contributor harmless for any liability
       incurred by, or claims asserted against, such Contributor by reason
       of your accepting any such warranty or additional liability.

    END OF TERMS AND CONDITIONS

    Copyright 2020 Thomas Leonard and others

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*)
