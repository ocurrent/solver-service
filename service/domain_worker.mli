type request = {
  packages : Packages.t;
  (** Packages from the main repository. *)

  root_pkgs : (OpamTypes.name * (OpamTypes.version * OpamFile.OPAM.t)) list;
  (** Name and contents of top-level opam files. *)

  pinned_pkgs : (OpamTypes.name * (OpamTypes.version * OpamFile.OPAM.t)) list;
  (** Name and contents of other pinned opam files. *)

  vars : Solver_service_api.Worker.Vars.t;
  (** Build platforms. *)
}

type reply = ((OpamPackage.t list, string) result * float, [`Msg of string]) result
(** [Ok (Ok selection)] if there is a solution.
    [Ok (Error msg)] if there is no solution.
    [Error msg] if the request was invalid. *)

val solve : request -> reply
