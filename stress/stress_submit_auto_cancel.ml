open Current.Syntax
open Pipeline_stages

let pipeline limit cluster time opam_repository_commit =
  Current.component "Make-requests"
  |> let** packages = opam_packages limit
     and* opam_repository_commit = opam_hash time opam_repository_commit ()
     and* platforms = platforms () in
     packages
     |> List.map (fun opam_pkg ->
            Current.ignore_value
            @@ analyze platforms opam_repository_commit cluster opam_pkg)
     |> Current.all

let v cluster limit time opam_repository_commits () =
  pipeline limit cluster time opam_repository_commits

let main config mode submission_uri limit auto_cancel_time
    opam_repository_commits =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let cluster = Current_ocluster.Connection.create submission_cap in
  let engine =
    Current.Engine.create ~config
      (v cluster limit auto_cancel_time opam_repository_commits)
  in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:"submit-stress-analysis"
      (Current_web.routes engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

open Cmdliner

let opam_repository_commits =
  Arg.value
  @@ Arg.opt
       (Arg.list ~sep:',' Arg.string)
       [
         "d0db19f581065173def5f086b8e0443f64609003";
         "5e382318663517d58b649820440cf9d41c50526a";
         "f18d48d31b11ad343aa285ad17ad55824f50d1b3";
       ]
  @@ Arg.info ~doc:"A list of opam opam-repository hash commits."
       ~docv:"COMMITS"
       [ "opam-repository-commits" ]

let submission_service =
  Arg.required
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The submission.cap file for the build scheduler service"
       ~docv:"FILE" [ "submission-service" ]

let request_limit =
  Arg.value
  @@ Arg.opt Arg.int 30
  @@ Arg.info ~doc:"The number of requests to send" ~docv:"N" [ "limit" ]

let repository_time =
  Arg.value
  @@ Arg.opt Arg.int 80
  @@ Arg.info
       ~doc:
         "Schedule how long an opam-repository commit remains, the new\n\
         \  opam-repository commit is from the list \
          $(b,opam-repository-commits). This scheduling triggers the \
          auto-cancelling of analysis jobs."
       ~docv:"N" [ "time" ]

let cmd =
  let doc =
    "Submit solve jobs to a scheduler that handles at least one solver-worker,\n\
    \    [opam-repository-commits] is a list "
  in
  let info = Cmd.info "stress_remote_auto_cancelling" ~doc in
  Cmd.v info
    Term.(
      term_result
        (const main
        $ Current.Config.cmdliner
        $ Current_web.cmdliner
        $ submission_service
        $ request_limit
        $ repository_time
        $ opam_repository_commits))

let () = Cmd.(exit @@ eval cmd)
