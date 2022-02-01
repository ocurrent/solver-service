open Lwt.Infix

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

let update_docker () =
  let image_name = Printf.sprintf "%s:%s" Self_update.repo Self_update.tag in
  Lwt_process.exec ("", [| "docker"; "pull"; image_name |])
  >|= check_exit_status
  >>= fun () ->
  Lwt_process.pread_line
    ( "",
      [|
        "docker";
        "image";
        "inspect";
        "-f";
        "{{ range index .RepoDigests }}{{ . }} {{ end }}";
        "--";
        image_name;
      |] )
  >|= fun new_repo_ids ->
  let new_repo_ids = Astring.String.cuts ~sep:" " new_repo_ids in
  let affix = Self_update.repo ^ "@" in
  match List.find_opt (Astring.String.is_prefix ~affix) new_repo_ids with
  | None -> Fmt.failwith "No new image starts with %S!" affix
  | Some id ->
      Logs.info (fun f -> f "Latest service version is %s" id);
      fun () ->
        Lwt_process.exec
          ( "",
            [|
              "docker"; "service"; "update"; "--image"; id; Self_update.service;
            |] )
        >|= check_exit_status

(* Respond to update requests by doing nothing, on the assumption that the
   admin has updated the local package version. *)
let update_normal () = Lwt.return (fun () -> Lwt.return ())

(* We extend the default build function to support solver jobs *)
let build ~solver ~switch ~log ~src:_ ~secrets:_ = function
  | `Custom c -> Solver_worker.solve ~solver ~switch ~log c
  | _ -> failwith "Only solver jobs are supported on this worker..."

let main default_level registration_path capacity name state_dir =
  setup_log default_level;
  let update =
    if Sys.file_exists "/.dockerenv" then update_docker else update_normal
  in
  Lwt_main.run
    (let vat = Capnp_rpc_unix.client_only_vat () in
     let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
     let solver = Solver_worker.spawn_local ~solver_dir:state_dir () in
     Cluster_worker.run ~build:(build ~solver) ~capacity ~name ~allow_push:[]
       ~state_dir ~update sr)

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
  Arg.value @@ Arg.opt Arg.int 10
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
  ( Term.(
      const main $ Logs_cli.level () $ connect_addr $ capacity $ worker_name
      $ state_dir),
    Term.info "ocluster-scheduler" ~doc ~man ~version )

let () = Term.(exit @@ eval cmd)
