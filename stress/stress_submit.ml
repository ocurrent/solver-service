open Current.Syntax
open Pipeline_stages

let pipeline limit cluster opam_repository_commit =
  Current.component "Make-requests"
  |> let** packages = opam_packages limit and* platforms = platforms () in
     packages
     |> List.map (fun opam_pkg ->
            Current.ignore_value
            @@ analyze platforms opam_repository_commit cluster opam_pkg)
     |> Current.all

let v cluster limit opam_repository_commit () =
  pipeline limit cluster opam_repository_commit

let main config mode submission_uri limit opam_repository_commit =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let cluster = Current_ocluster.Connection.create submission_cap in
  let engine =
    Current.Engine.create ~config (v cluster limit opam_repository_commit)
  in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:"submit-stress-analysis"
      (Current_web.routes engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

open Cmdliner

let opam_repository_commit =
  Arg.value
  @@ Arg.opt Arg.string "a9fb5a379794b0d5d7f663ff3a3bed5d4672a5d3"
  @@ Arg.info ~doc:"The hash commit of opam-repository." ~docv:"COMMIT"
       [ "opam-repository-commit" ]

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
  let doc =
    "Submit solve jobs to a scheduler that handles at least one solver-worker"
  in
  let info = Cmd.info "stress_remote" ~doc in
  Cmd.v info
    Term.(
      term_result
        (const main
        $ Current.Config.cmdliner
        $ Current_web.cmdliner
        $ submission_service
        $ request_limit
        $ opam_repository_commit))

let () = Cmd.(exit @@ eval cmd)
