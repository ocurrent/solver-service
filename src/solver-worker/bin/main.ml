let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ();
  ()

let or_die = function Ok x -> x | Error (`Msg m) -> failwith m

let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x -> Fmt.failwith "Sub-process failed with exit code %d" x
  | Unix.WSIGNALED x -> Fmt.failwith "Sub-process failed with signal %d" x
  | Unix.WSTOPPED x -> Fmt.failwith "Sub-process stopped with signal %d" x

module Self_update = struct
  let service = "builder_agent"
  let repo = "ocurrent/solver-worker"
  let tag = "live"
end

let build ~solver ~switch ~log ~src:_ ~secrets:_ c =
  Solver_worker.solve ~solver ~switch ~log c

let main default_level registration_path capacity name state_dir =
  setup_log default_level;
  Lwt_main.run
    (let vat = Capnp_rpc_unix.client_only_vat () in
     let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
     let solver = Solver_worker.spawn_local ~solver_dir:state_dir () in
     Worker.run ~build:(build ~solver) ~capacity ~name ~state_dir sr)

open Cmdliner

let worker_name =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Unique builder name" ~docv:"ID" [ "name" ]

let connect_addr =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of register.cap from build-scheduler" ~docv:"ADDR"
       [ "c"; "connect" ]

let capacity =
  Arg.value
  @@ Arg.opt Arg.int 10
  @@ Arg.info ~doc:"The number of builds that can run in parallel" ~docv:"N"
       [ "capacity" ]

let state_dir =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Directory for caches, etc (e.g. /var/lib/ocluster-worker)"
       ~docv:"PATH" [ "state-dir" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "An OCluster worker for solving opam dependencies" in
  let man =
    [
      `P
        "Connect to an OCluster sheduler pool and submit custom jobs to solver \
         opam dependencies.";
    ]
  in
  let info = Cmd.info "ocluster-scheduler" ~doc ~man ~version in
  Cmd.v info
    Term.(
      const main
      $ Logs_cli.level ()
      $ connect_addr
      $ capacity
      $ worker_name
      $ state_dir)

let () = Cmd.(exit @@ eval cmd)
