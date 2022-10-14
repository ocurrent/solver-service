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

module Service = Solver_service.Service.Make (Mock_opam_repo)

let test_good_packages _sw () =
  let proc = Procress.const_response ~response:"+lwt.5.5.0 yaml.3.0.0" in
  let log = Buffer.create 100 in
  let req =
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commit = "95d27ad970057f68179577594813dc1828324a2f";
        root_pkgs = [];
        pinned_pkgs = [];
        platforms = [];
      }
  in
  let+ process =
    Service.Epoch.process ~log:(job_log log) ~id:"unique-id" req proc
  in
  Alcotest.(check (result (list string) string))
    "Same packages"
    (Ok [ "lwt.5.5.0"; "yaml.3.0.0" ])
    process

let test_error _sw () =
  let msg = "Something went wrong!" in
  let proc = Procress.const_response ~response:("-" ^ msg) in
  let log = Buffer.create 100 in
  let req =
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commit = "95d27ad970057f68179577594813dc1828324a2f";
        root_pkgs = [];
        pinned_pkgs = [];
        platforms = [];
      }
  in
  let+ process =
    Service.Epoch.process ~log:(job_log log) ~id:"unique-id" req proc
  in
  Alcotest.(check (result (list string) string))
    "Same packages" (Error msg) process

let solver_response =
  Alcotest.of_pp (fun ppf t ->
      Yojson.Safe.pp ppf (Solver_service_api.Worker.Solve_response.to_yojson t))

let test_e2e _sw () =
  Lwt_io.with_temp_dir @@ fun dir ->
  let* _store = Mock_opam_repo.setup_store (Ok (Fpath.v dir)) in
  let* commit = Mock_opam_repo.commit in
  let os_id = "testOS" in
  let* vars =
    Utils.get_vars ~ocaml_package_name:"ocaml" ~ocaml_version:"4.13.1" ()
  in
  let create_worker _hash =
    Procress.const_response ~response:"+lwt.5.5.0 yaml.3.0.0"
  in
  let log = Buffer.create 100 in
  let req =
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commit = commit;
        root_pkgs = [ ("yaml.3.0.0", "") ];
        pinned_pkgs = [];
        platforms = [ (os_id, vars) ];
      }
  in
  let* service = Service.v ~n_workers:1 ~create_worker in
  let+ response =
    Solver_service_api.Solver.solve ~log:(job_log log) service req
  in
  Alcotest.(check solver_response)
    "Same solve reponse"
    (Ok
       [
         {
           id = os_id;
           packages = [ "lwt.5.5.0"; "yaml.3.0.0" ];
           compat_pkgs = [ "yaml.3.0.0" ];
           commit;
         };
       ])
    response

let tests =
  Alcotest_lwt.
    [
      test_case "good-packages" `Quick test_good_packages;
      test_case "error-handling" `Quick test_error;
      test_case "end-to-end" `Quick test_e2e;
    ]
