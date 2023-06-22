module Make (_ : Opam_repository_intf.S) : sig
  module Epoch : sig
    type t
    (* An Epoch handles all requests for a single opam-repository HEAD commit. *)

    val create :
      n_workers:int ->
      create_worker:(Remote_commit.t list -> Internal_worker.Solver_process.t) ->
      Remote_commit.t list ->
      t Lwt.t

    val process :
      switch:Lwt_switch.t ->
      log:Solver_service_api.Solver.Log.X.t Capnp_rpc_lwt.Capability.t ->
      id:string ->
      request_uid:string ->
      Solver_service_api.Worker.Solve_request.t ->
      Internal_worker.Solver_process.t ->
      (string list, string) result Lwt.t
    (** [process ~log ~id request process] will write the [request] to the stdin
        of [procress] and read [stdout] returning the packages. Information is
        logged into [log] with [id]. *)

    val dispose : t -> unit Lwt.t
  end

  val handle :
    switch:Lwt_switch.t ->
    (Epoch.t, Remote_commit.t list) Epoch_lock.t ->
    log:Solver_service_api.Solver.Log.X.t Capnp_rpc_lwt.Capability.t ->
    Solver_service_api.Worker.Solve_request.t ->
    Solver_service_api.Worker.Selection.t list Lwt.t

  val v :
    n_workers:int ->
    create_worker:(Remote_commit.t list -> Internal_worker.Solver_process.t) ->
    Solver_service_api.Solver.t Lwt.t
  (** [v ~n_workers ~create_worker] is a solver service that distributes work to
      up to [n_workers] subprocesses, using [create_worker hash] to spawn new
      workers. *)
end
