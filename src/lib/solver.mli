val main : Git_unix.Store.Hash.t -> unit
(** [main hash] runs a worker process that reads requests from stdin and writes
    results to stdout, using commit [hash] in opam-repository. *)

val spawn_local : solver_dir:string -> Solver_service_api.Solver.t
(** [spawn_local ~solver_dir] spawns a child process solver instance. *)