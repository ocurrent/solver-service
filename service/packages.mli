type t
(** A cache of opam files for a particular Git commit (or set of commits). *)

val empty : t

val of_commit :
  ?super:t ->
  Safe_store.t ->
  Git_unix.Store.Hash.t ->
  t
(** [of_commit store commit] is an index of the opam files in [store] at [commit].

    This must be called from the Lwt domain.

    @param super The new index overlays [super]. *)

val get_versions : t -> OpamPackage.Name.t -> OpamFile.OPAM.t OpamPackage.Version.Map.t
(** [get_versions t name] returns the versions of [name] in [t].

    This can be called from any domain. *)
