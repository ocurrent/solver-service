let solve_to_custom req =
  let open Cluster_api.Raw in
  let params =
    Yojson.Safe.to_string
    @@ Solver_service_api.Worker.Solve_request.to_yojson req
  in
  let custom = Builder.Custom.init_root () in
  let builder = Builder.Custom.payload_get custom in
  let request =
    Solver_service_api.Raw.Builder.Solver.Solve.Params.init_pointer builder
  in
  Solver_service_api.Raw.Builder.Solver.Solve.Params.request_set request params;
  let r = Reader.Custom.of_builder custom in
  Reader.Custom.payload_get r

let solve_of_custom c =
  let open Solver_service_api.Raw in
  let payload = Cluster_api.Custom.payload c in
  let r = Reader.of_pointer payload in
  let request = Reader.Solver.Solve.Params.request_get r in
  Solver_service_api.Worker.Solve_request.of_yojson
  @@ Yojson.Safe.from_string request

(* Copy log messages from the solver to [log]. *)
let cluster_worker_log log =
  let module L = Solver_service_api.Raw.Service.Log in
  L.local @@ object
    inherit L.service

    method write_impl params release_param_caps =
      let open L.Write in
      release_param_caps ();
      let msg = Params.msg_get params in
      Log_data.write log msg;
      Capnp_rpc_lwt.Service.return_empty ()
  end

let solve ~cancelled ~solver ~log c =
  match solve_of_custom c with
  | Error m -> failwith m
  | Ok request ->
    let log = cluster_worker_log log in
    let selections =
      Lwt_eio.run_lwt @@ fun () ->
      Capnp_rpc_lwt.Capability.with_ref log @@ fun log ->
      Lwt_eio.run_eio @@ fun () ->
      Solver_service.Solver.solve ~cancelled solver ~log request
    in
    begin match selections with
      | Ok sels ->
        Log.info (fun f -> f "Job succeeded (found solutions for %d platforms)" (List.length sels));
      | Error `Msg m ->
        Log.info (fun f -> f "Job failed: %s" m);
      | Error `Cancelled ->
        Log.info (fun f -> f "Job cancelled");
    end;
    Yojson.Safe.to_string
    @@ Solver_service_api.Worker.Solve_response.to_yojson selections
