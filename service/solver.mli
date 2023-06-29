type t

val create :
  sw:Eio.Switch.t ->
  domain_mgr:#Eio.Domain_manager.t ->
  process_mgr:#Eio.Process.mgr ->
  cache_dir:string ->
  n_workers:int ->
  t
(** [create ~sw ~domain_mgr ~process_mgr ~cache_dir ~n_workers] is a solver service that
    distributes work between [n_workers] domains.

    @param sw Holds the worker domains.
    @param domain_mgr Used to spawn new domains.
    @param process_mgr Used to run the "git" command.
    @param cache_dir Directory for local git clones.
    @param n_workers Maximum number of worker domains. *)

val solve :
  t ->
  log:Solver_service_api.Solver.Log.t ->
  Solver_service_api.Worker.Solve_request.t ->
  Solver_service_api.Worker.Solve_response.t
