(** An OCluster worker that can solve requests being submitted to OCluster as a
    custom job specification. *)

val solve_to_custom :
  Solver_service_api.Worker.Solve_request.t -> Cluster_api.Custom.payload
(** [solve_to_custom req] converts the solver request to a custom job
    specification. *)

val solve_of_custom :
  Solver_service_api.Raw.Reader.pointer_t Cluster_api.Custom.t ->
  (Solver_service_api.Worker.Solve_request.t, string) result
(** [solve_of_custom c] tries to read the custom job specification as a solver
    request. *)

val solve :
  cancelled:unit Eio.Promise.t ->
  solver:Solver_service.Solver.t ->
  log:Log_data.t ->
  Solver_service_api.Raw.Reader.pointer_t Cluster_api.Custom.t ->
  string
(** [solve ~cancelled ~solver ~log c] interprets [c] as a solver request and
    solves it using [solver]. *)
