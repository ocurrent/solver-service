module Epoch : sig
  val process :
    log:Solver_service_api.Solver.Log.X.t Capnp_rpc_lwt.Capability.t ->
    id:string ->
    Solver_service_api.Worker.Solve_request.t ->
    < stdin : Lwt_io.output_channel ; stdout : Lwt_io.input_channel ; .. > ->
    (string list, string) result Lwt.t
  (** [process ~log ~id request process] will write the [request] to the stdin
      of [procress] and read [stdout] returning the packages. Information is
      logged into [log] with [id]. *)
end

val v :
  n_workers:int ->
  create_worker:(Git_unix.Store.Hash.t -> Lwt_process.process) ->
  Solver_service_api.Solver.t Lwt.t
(** [v ~n_workers ~create_worker] is a solver service that distributes work to
    up to [n_workers] subprocesses, using [create_worker hash] to spawn new
    workers. *)
