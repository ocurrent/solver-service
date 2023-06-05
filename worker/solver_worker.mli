(** An OCluster worker that can solve requests being submitted to OCluster as a custom job specification. *)

module Log_data = Log_data
module Context = Context
module Log = Log
module Process = Process

module Solver_request : sig
  type t
  (* A pool of subprocesses as internal-workers used for handling solver request *)

  val create : n_workers:int -> unit -> t
  (** [create ~n_workers ()] will create a pool of [n_workers] subprocesses as
      internal-workers *)

  val solve :
    t ->
    switch:Lwt_switch.t ->
    log:Solver_service_api.Solver.Log.X.t Capnp_rpc_lwt.Capability.t ->
    request:Solver_service_api.Worker.Solve_request.t ->
    Solver_service_api.Worker.Solve_response.t Lwt.t
  (**[solve t ~switch ~log ~request] will solve the [request] using the pool
     [t], the [request] will be distributed among the internal-workers of the
     pool.*)
end

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
  solver:Solver_request.t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  Solver_service_api.Raw.Reader.pointer_t Cluster_api.Custom.t ->
  (string, [ `Cancelled | `Msg of string ]) Lwt_result.t
(** [handle ~solver ~switch ~log c] interprets [c] as a solver request and
    solves it using [solver]. *)
