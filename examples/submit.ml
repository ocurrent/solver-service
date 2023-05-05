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

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter reporter;
  ()

let opam_repository =
  { Current_github.Repo_id.owner = "ocaml"; name = "opam-repository" }

let repo = { Current_github.Repo_id.owner = "ocurrent"; name = "obuilder" }
let pool = "solver"

module Current_solve = struct
  module Op = struct
    open Lwt.Infix

    type t = Current_ocluster.Connection.t

    let id = "mock-ocluster-build"

    (* Build Pool *)
    module Key = Current.String

    (* Dockerfile Spec *)
    module Value = struct
      open Solver_service_api.Worker

      type t = Solve_request.t

      let digest t = Solve_request.to_yojson t |> Yojson.Safe.to_string
      let pp ppf t = Yojson.Safe.pp ppf (Solve_request.to_yojson t)
    end

    module Outcome = Current.String

    let pp = Fmt.(pair string Value.pp)
    let auto_cancel = true
    let latched = true

    let run t job pool value =
      let action =
        Cluster_api.Submission.custom_build
        @@ Cluster_api.Custom.v ~kind:"solve"
        @@ Solve.solve_to_custom value
      in
      let build_pool =
        Current_ocluster.Connection.pool ~job ~pool ~action ~cache_hint:"" t
      in
      Current.Job.start_with ~pool:build_pool job ~level:Current.Level.Average
      >>= fun build_job ->
      Capnp_rpc_lwt.Capability.with_ref build_job
        (Current_ocluster.Connection.run_job ~job)
  end

  module BC = Current_cache.Generic (Op)

  let solve t pool request =
    let open Current.Syntax in
    Current.component "custom cluster solver"
    |> let> request in
       BC.run t pool request
end

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
  let request =
    let+ opamfiles = Solve.get_opamfile ~packages src
    and* opam_repo =
      Current_github.Api.Anonymous.head_of opam_repository
        (`Ref "refs/heads/master")
    in
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commits =
          [
            ( Current_git.Commit_id.repo opam_repo,
              Current_git.Commit_id.hash opam_repo );
          ];
        root_pkgs = opamfiles;
        pinned_pkgs = [];
        platforms = [ ("os", vars) ];
        lower_bound = false;
      }
  in
  let selection =
    let+ response = Current_solve.solve cluster pool request in
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
        | Error (`Msg m) -> failwith m
        | Error `Cancelled -> Fmt.failwith "Job cancelled")
    | Error m -> failwith m
  in
  selection

let main () config mode submission_uri =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let vars =
    Lwt_main.run
    @@ Solve.get_vars ~ocaml_package_name:"obuilder" ~ocaml_version:"4.13.1" ()
  in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let cluster = Current_ocluster.Connection.create submission_cap in
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

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd =
  let doc = "Run a custom solver job in the cluster." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info
    Term.(
      term_result
        (const main
        $ setup_log
        $ Current.Config.cmdliner
        $ Current_web.cmdliner
        $ submission_service))

let () = Cmd.(exit @@ eval cmd)
