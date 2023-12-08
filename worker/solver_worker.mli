open Capnp_rpc_lwt

val run :
  cacheable: bool ->
  name:string ->
  capacity:int ->
  Solver_service.Solver.t ->
  Cluster_api.Registration.X.t Sturdy_ref.t -> 'a
