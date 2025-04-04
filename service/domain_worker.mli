type request = {
  packages : Packages.t;
  (** Packages from the main repository. *)

  root_pkgs : (OpamTypes.name * (OpamTypes.version * OpamFile.OPAM.t)) list;
  (** Name and contents of top-level opam files. *)

  pinned_pkgs : (OpamTypes.name * (OpamTypes.version * OpamFile.OPAM.t)) list;
  (** Name and contents of other pinned opam files. *)

  vars : Solver_service_api.Worker.Vars.t;
  (** Build platforms. *)

  cancelled : unit Eio.Promise.t option;
  (** If resolved, the result is not needed. *)
}

type reply = ((OpamPackage.t list, string) result * float, [`Msg of string | `Cancelled]) result
(** [Ok (Ok selection)] if there is a solution.
    [Ok (Error msg)] if there is no solution.
    [Error msg] if the request was invalid.
    [Error Cancelled] if the request was cancelled before started. *)

val env : Solver_service_api.Worker.Vars.t -> string -> OpamVariable.variable_contents option
(** [env vars name] is the value of [name] in [vars]. *)

val solve : request -> reply
