open Eio.Std
module Worker = Solver_service_api.Worker

(** Cache system where try to hit in a Sqlite3 database *)

type t

val create : cache_dir: string -> proc_mgr: [`Generic] Eio.Process.mgr_ty r -> t

val solve :
  t ->
  solve:(log:Solver_service_api.Solver.Log.t -> Worker.Solve_request.t -> Worker.Solve_response.t) ->
  Solver_service_api.Solver.Log.t ->
  Worker.Solve_request.t ->
  Worker.Solve_response.t
(** [solve t ~solve log request] try to hit the cache, if missed it uses [t] and [solve] to solve the [request]

    @param log Diagnostics about failed solves (and other logging) goes here. *)
