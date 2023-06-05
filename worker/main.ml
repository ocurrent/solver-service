let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Prometheus_unix.Logging.init ?default_level:level ();
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let or_die = function Ok x -> x | Error (`Msg m) -> failwith m

let build ~solver ~switch ~log ~src:_ ~secrets:_ c =
  Solver_worker.solve ~solver ~switch ~log c

let main () registration_path capacity internal_workers name state_dir =
  Lwt_main.run begin
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
      let solver =
        Solver_worker.Solver_request.create ~n_workers:internal_workers ()
      in
      Worker.run ~build:(build ~solver) ~capacity ~name ~state_dir sr
    end

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let worker_name =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Unique worker name" ~docv:"ID" [ "name" ]

let connect_addr =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of register.cap from build-scheduler" ~docv:"ADDR"
       [ "c"; "connect" ]

let capacity =
  Arg.value
  @@ Arg.opt Arg.int 15
  @@ Arg.info ~doc:"The number of builds that can run in parallel" ~docv:"N"
       [ "capacity" ]

let internal_workers =
  Arg.value
  @@ Arg.opt Arg.int 30
  @@ Arg.info
       ~doc:
         "The number of sub-processes solving requests in parallel by the \
          solver-service. One build\n\
         \         job ($(b,--capacity)) could take more sub-processes (a \
          build job can have\n\
         \         more than one target platform)."
       ~docv:"N" [ "internal-workers" ]

let state_dir =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Directory for caches, etc (e.g. /var/lib/solver-worker)"
       ~docv:"PATH" [ "state-dir" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "An OCluster worker for solving opam dependencies." in
  let man =
    [
      `P
        "Connect to an OCluster scheduler pool and submit custom jobs to solve \
         opam dependencies.";
    ]
  in
  let info = Cmd.info "solver-worker" ~doc ~man ~version in
  Cmd.v info
    Term.(
      const main
      $ setup_log
      $ connect_addr
      $ capacity
      $ internal_workers
      $ worker_name
      $ state_dir)

let () = Cmd.(exit @@ eval cmd)
