let program_name = "solver_pipeline"

open Current.Syntax

let reporter =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout
      ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let init ?(level = Logs.Info) () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some level);
  Logs.set_reporter reporter

let opam_repository =
  { Current_github.Repo_id.owner = "ocaml"; name = "opam-repository" }

let repo = { Current_github.Repo_id.owner = "ocurrent"; name = "obuilder" }
let pool = "solver"
let timeout = Duration.of_min 60

let pipeline ~cluster vars () =
  let src =
    let+ src =
      Current_github.Api.Anonymous.head_of repo (`Ref "refs/heads/master")
    in
    [ src ]
  in
  let packages =
    [
      ("obuilder-spec.dev", Fpath.v "obuilder-spec.opam");
      ("obuilder.dev", Fpath.v "obuilder.opam");
    ]
  in
  let cluster = Current_ocluster.with_timeout (Some timeout) cluster in
  let request =
    let+ opamfiles = Solve.get_opamfile ~packages src
    and* opam_repo =
      Current_github.Api.Anonymous.head_of opam_repository
        (`Ref "refs/heads/master")
    in
    let payload =
      Solve.solve_to_custom
        Solver_service_api.Worker.Solve_request.
          {
            opam_repository_commit = Current_git.Commit_id.hash opam_repo;
            root_pkgs = opamfiles;
            pinned_pkgs = [];
            platforms = [ ("os", vars) ];
          }
    in
    Cluster_api.Custom.v ~kind:"solve" payload
  in
  let selection =
    let+ response =
      Current_ocluster.custom ~label:"solver" cluster ~src ~pool request
    in
    match
      Solver_service_api.Worker.Solve_response.of_yojson
        (Yojson.Safe.from_string response)
    with
    | Ok response -> (
        match response with
        | Ok (selection :: _) ->
            let packages = selection.packages in
            Logs.info (fun f -> f "%s" (String.concat " " packages))
        | Ok [] -> failwith "No packages found"
        | Error (`Msg m) -> failwith m)
    | Error m -> failwith m
  in
  selection

let main config mode submission_uri =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let vars =
    Lwt_main.run
    @@ Solve.get_vars ~ocaml_package_name:"obuilder" ~ocaml_version:"4.13.1" ()
  in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let connection = Current_ocluster.Connection.create submission_cap in
  let cluster = Current_ocluster.v connection in
  let engine = Current.Engine.create ~config (pipeline ~cluster vars) in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:program_name
      (Current_web.routes engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

(* Command-line parsing *)

open Cmdliner

let submission_service =
  Arg.required
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The submission.cap file for the build scheduler service"
       ~docv:"FILE" [ "submission-service" ]

let cmd =
  let doc = "Run a custom solver job and a Docker build on a cluster." in
  ( Term.(
      term_result
        (const main $ Current.Config.cmdliner $ Current_web.cmdliner
       $ submission_service)),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
