(** An OCluster worker that can solve requests being submitted to OCluster as a
    custom job specification. *)

val solve :
  cancelled:unit Eio.Promise.t ->
  solver:Solver_service.Solver.t ->
  log:Log_data.t ->
  Solver_service_api.Raw.Reader.pointer_t Cluster_api.Custom.t ->
  string
(** [solve ~cancelled ~solver ~log c] interprets [c] as a solver request and
    solves it using [solver]. *)
