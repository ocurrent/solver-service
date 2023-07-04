(* This example assumes the solver service to be running *)
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

(* ~~~ Solver client ~~~ *)
let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let opam_template arch =
  let arch = Option.value ~default:"%{arch}%" arch in
  Fmt.str
    {|
  {
    "arch": "%s",
    "os": "%%{os}%%",
    "os_family": "%%{os-family}%%",
    "os_distribution": "%%{os-distribution}%%",
    "os_version": "%%{os-version}%%",
    "opam_version": "%%{opam-version}%%"
  }
|}
    arch

let get_vars ~ocaml_package_name ~ocaml_version ?arch ?(lower_bound = false) ()
    =
  let+ vars =
    Solver_service.Process.pread
      ("", [| "opam"; "config"; "expand"; opam_template arch |])
  in
  let json =
    match Yojson.Safe.from_string vars with
    | `Assoc items ->
        `Assoc
          (("ocaml_package", `String ocaml_package_name)
          :: ("ocaml_version", `String ocaml_version)
          :: ("lower_bound", `Bool lower_bound)
          :: items)
    | json ->
        Fmt.failwith "Unexpected JSON: %a"
          Yojson.Safe.(pretty_print ~std:true)
          json
  in
  Result.get_ok @@ Solver_service_api.Worker.Vars.of_yojson json

let get_opam_file pv =
  Solver_service.Process.pread ("", [| "opam"; "show"; "--raw"; pv |])

let run_client ~package ~version ~ocaml_version ~opam_commit service =
  let pv = package ^ "." ^ version in
  let* platform =
    get_vars ~ocaml_package_name:"ocaml-base-compiler" ~ocaml_version ()
  in
  let* opam_file = get_opam_file pv in
  let request =
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commits =
          [ ("https://github.com/ocaml/opam-repository.git", opam_commit) ];
        root_pkgs = [ (pv, opam_file) ];
        pinned_pkgs = [];
        platforms = [ (platform.os, platform) ];
      }
  in
  let job = Buffer.create 100 in
  Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
  let+ response = Solver_service_api.Solver.solve service ~log request in
  match response with
  | Ok selection ->
      let packages = (List.hd @@ selection).packages in
      Fmt.pr "opam install %a" Fmt.(list ~sep:(Fmt.any " ") string) packages
  | Error (`Msg m) ->
      print_endline (Buffer.contents job);
      Fmt.failwith "Solver service failed with: %s" m

let connect package version ocaml_version opam_commit uri =
  Lwt_main.run
    (let client_vat = Capnp_rpc_unix.client_only_vat () in
     let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
     Capnp_rpc_unix.with_cap_exn sr
       (run_client ~package ~version ~ocaml_version ~opam_commit))

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let ocaml_version =
  Arg.value
  @@ Arg.(opt string "4.14.0")
  @@ Arg.info [ "ocaml-version" ] ~docv:"OCAML-VERSION"
       ~doc:"A version of OCaml to use for solving (e.g. 4.13.0)"

let opam_commit =
  Arg.value
  @@ Arg.(opt string "61c80509aec809b94b4ff7a505235f1ba605c756")
  @@ Arg.info [ "commit" ] ~docv:"COMMIT"
       ~doc:"The commit SHA to use for opam-repository"

let package =
  Arg.required
  @@ Arg.(opt (some string) None)
  @@ Arg.info [ "package" ] ~docv:"PACKAGE"
       ~doc:"The package name to solve for (e.g. yaml)"

let version =
  Arg.required
  @@ Arg.(opt (some string) None)
  @@ Arg.info [ "version" ] ~docv:"VERSION"
       ~doc:"The version of the package to solve for (e.g. 3.0.0)"

let connect_cmd =
  let doc = "Solve a simple package.version request using a solver-service" in
  let info = Cmd.info "solve-local" ~doc in
  Cmd.v info
    Term.(
      const connect
      $ package
      $ version
      $ ocaml_version
      $ opam_commit
      $ connect_addr)

let () = Cmd.(exit @@ eval connect_cmd)
