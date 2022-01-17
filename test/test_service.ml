open Lwt.Syntax

let job_log job =
  let module L = Solver_service_api.Raw.Service.Log in
  L.local
  @@ object
       inherit L.service

       method write_impl params release_param_caps =
         let open L.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Buffer.add_string job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

module Procress = struct
  let const_response ~response : Lwt_process.process =
    object
      val std_out =
        let time = 1000 in
        let length = String.length response in
        let output = Fmt.str "%i\n%d\n%s" time length response in
        Lwt_io.of_bytes ~mode:Input (Lwt_bytes.of_string output)

      method pid = 10
      method rusage = Lwt.return Lwt_unix.{ ru_utime = 0.; ru_stime = 0. }
      method state = Lwt_process.Running
      method stdin = Lwt_io.null
      method stdout = std_out
      method terminate = ()
      method status = Lwt.return (Unix.WEXITED 0)
      method close = Lwt.return (Unix.WEXITED 0)
      method kill _i = ()
    end
end

let test _sw () =
  let proc = Procress.const_response ~response:"+lwt.5.5.0 yaml.3.0.0" in
  let log = Buffer.create 100 in
  let req =
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commit = "abcdef";
        root_pkgs = [];
        pinned_pkgs = [];
        platforms = [];
      }
  in
  let+ process =
    Solver_service.Service.Epoch.process ~log:(job_log log) ~id:"unique-id" req
      proc
  in
  Alcotest.(check (result (list string) string))
    "Same packages"
    (Ok [ "lwt.5.5.0"; "yaml.3.0.0" ])
    process

let tests = [ Alcotest_lwt.test_case "solve" `Quick test ]
