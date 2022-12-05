(* This stress test is for the underlying solver-service library
   that the workers use to solve dependencies. *)
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

let ocaml_package_name = "ocaml-base-compiler"
let ocaml_version = "4.14.0"
let opam_repository_commit = "34576e67c88137d40ce6ff9e252d549e9e87205f"

let package_to_custom vars package =
  let+ opamfile = Utils.get_opam_file package in
  Solver_service_api.Worker.Solve_request.
    {
      opam_repository_commits =
        [ ("git@github.com:ocaml/opam-repository.git", opam_repository_commit) ];
      root_pkgs = [ (package, opamfile) ];
      pinned_pkgs = [];
      platforms = [ ("macOS", vars); ("linux", vars); ("windows", vars) ];
    }

let requests log solver =
  let* vars = Utils.get_vars ~ocaml_package_name ~ocaml_version () in
  let packages = [ "fmt.0.9.0"; "alcotest.1.6.0"; "yojson.2.0.2" ] in
  let* requests = Lwt_list.map_p (package_to_custom vars) packages in
  let before = Unix.gettimeofday () in
  let* results =
    Lwt_list.map_p (Solver_service_api.Solver.solve solver ~log) requests
  in
  let+ () =
    Lwt_list.iter_s
      (fun r ->
        Lwt.return
        @@ Yojson.Safe.pp Format.std_formatter
        @@ Solver_service_api.Worker.Solve_response.to_yojson r)
      results
  in
  before

let stress () =
  let client_vat = Capnp_rpc_unix.client_only_vat () in
  let sr =
    Capnp_rpc_unix.Vat.import_exn client_vat (Uri.of_string Sys.argv.(1))
  in
  let job = Buffer.create 1000 in
  Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
  Capnp_rpc_unix.with_cap_exn sr (requests log)

let () =
  let before = Lwt_main.run (stress ()) in
  let after = Unix.gettimeofday () in
  Format.printf "\nSolved requests in: %f" (after -. before)
