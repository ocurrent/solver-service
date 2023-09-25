module Worker = Solver_service_api.Worker

let v t =
  let open Capnp_rpc_lwt in
  let module X = Solver_service_api.Raw.Service.Solver in
  X.local
  @@ object
    inherit X.service

    method solve_impl params release_param_caps =
      let open X.Solve in
      let request = Params.request_get params in
      let log = Params.log_get params in
      release_param_caps ();
      match log with
      | None -> Service.fail "Missing log argument!"
      | Some log ->
        Capnp_rpc_lwt.Service.return_lwt @@ fun () ->
        Capability.with_ref log @@ fun log ->
        match
          Worker.Solve_request.of_yojson
            (Yojson.Safe.from_string request)
        with
        | Error msg ->
          Lwt_result.fail
            (`Capnp
               (Capnp_rpc.Error.exn "Bad JSON in request: %s" msg))
        | Ok request ->
          Lwt_eio.run_eio @@ fun () ->
          let selections = Solver.solve t ~log request in
          let json =
            Yojson.Safe.to_string
              (Worker.Solve_response.to_yojson selections)
          in
          let response, results =
            Capnp_rpc_lwt.Service.Response.create Results.init_pointer
          in
          Results.response_set results json;
          Ok response
  end
