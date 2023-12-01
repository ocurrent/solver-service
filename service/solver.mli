type t

val create :
  sw:Eio.Switch.t ->
  domain_mgr:_ Eio.Domain_manager.t ->
  process_mgr:_ Eio.Process.mgr ->
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
  ?cacheable:bool ->
  ?cancelled:unit Eio.Promise.t ->
  t ->
  log:Solver_service_api.Solver.Log.t ->
  Solver_service_api.Worker.Solve_request.t ->
  Solver_service_api.Worker.Solve_response.t
(** [solve t ~log request] uses [t] to solve for all the platforms in [request].

    @param cancelled Try to cancel the job when this is resolved.
    @param log Diagnostics about failed solves (and other logging) goes here. *)
