open Lwt.Infix
open Lwt.Syntax

let solve_to_custom req builder =
  let params =
    Yojson.Safe.to_string
    @@ Solver_service_api.Worker.Solve_request.to_yojson req
  in
  let builder =
    Solver_service_api.Raw.Builder.Solver.Solve.Params.init_pointer builder
  in
  Solver_service_api.Raw.Builder.Solver.Solve.Params.request_set builder params

let solve cluster job request =
  let action =
    Cluster_api.Submission.custom_build
    @@ Cluster_api.Custom.v ~kind:"solve"
    @@ solve_to_custom request
  in
  let build_pool =
    Current_ocluster.Connection.pool ~job ~pool:"solver" ~action ~cache_hint:""
      cluster
  in
  Current.Job.start_with ~pool:build_pool job ~level:Current.Level.Average
  >>= fun build_job ->
  Capnp_rpc_lwt.Capability.with_ref build_job
    (Current_ocluster.Connection.run_job ~job)

let ocaml_package_name = "ocaml-base-compiler"
let ocaml_version = "4.14.0"
let opam_repository_commit = "34576e67c88137d40ce6ff9e252d549e9e87205f"

let make_requests limit =
  let* vars = Utils.get_vars ~ocaml_package_name ~ocaml_version () in
  let* opam_packages = Utils.get_opam_packages () in
  let rec requests packages acc n =
    match packages with
    | [] -> Lwt.return acc
    | package :: tl ->
        if n >= limit then Lwt.return acc
        else
          let* opamfile = Utils.get_opam_file package in
          let request =
            Solver_service_api.Worker.Solve_request.
              {
                opam_repository_commits =
                  [
                    ( "https://github.com/ocaml/opam-repository.git",
                      opam_repository_commit );
                  ];
                root_pkgs = [ (package, opamfile) ];
                pinned_pkgs = [];
                platforms =
                  [ ("macOS", vars); ("linux", vars); ("windows", vars) ];
              }
          in
          requests tl (request :: acc) (n + 1)
  in
  requests opam_packages [] 0

let switch = Current.Switch.create ~label:"solve-remote" ()
let config = Current.Config.v ()

let stress cluster limit =
  let* requests = make_requests limit in
  let before = Unix.gettimeofday () in
  let+ results =
    Lwt_list.map_p
      (fun request ->
        let job = Current.Job.create ~label:"solve-job" ~switch ~config () in
        solve cluster job request)
      requests
  in
  (before, List.length results)

let main submission_uri limit =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let cluster = Current_ocluster.Connection.create submission_cap in
  let before, requested = Lwt_main.run (stress cluster limit) in
  let after = Unix.gettimeofday () in
  ignore (Current.Switch.turn_off switch);
  Format.printf "\nSolved %d requests in: %f\n" requested (after -. before)

open Cmdliner

let submission_service =
  Arg.required
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The submission.cap file for the build scheduler service"
       ~docv:"FILE" [ "submission-service" ]

let request_limit =
  Arg.value
  @@ Arg.opt Arg.int 30
  @@ Arg.info ~doc:"The number of requests to send" ~docv:"N" [ "limit" ]

let cmd =
  let doc = "Submit solve jobs to a scheduler that handles a solver-worker" in
  let info = Cmd.info "stress_remote" ~doc in
  Cmd.v info Term.(const main $ submission_service $ request_limit)

let () = Cmd.(exit @@ eval cmd)
