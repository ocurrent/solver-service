(** Safe wrapper around [Git_unix.Store].

    This module is only used from the main domain. *)

type t
(** Contains a Git_unix.Store, which may be replaced (they are invalidated by doing "git fetch") *)

type request

val create : unit -> t
(** [create ()] is a fresh store container. Initially, {!get_store} returns an error. *)

val get : sw:Eio.Switch.t -> t -> request
(** [get ~sw t] locks the current version of the store until [sw] finishes (or {!upgrade} is called). *)

val get_store : request -> (Git_unix.Store.t, exn) result
(** [get_store request] returns the Git store in [request]. *)

val upgrade : request -> (unit -> Git_unix.Store.t) -> (unit, [> `Concurrent_upgrade | `Exn of exn]) result
(** [update request fn] uses [fn ()] to update to a new store.

    It first waits for all users of the old store to finish.

    The old [request] is released during this process.

    [Error `Concurrent_upgrade] indicates that [request] is no longer the latest generation.
    The caller should call {!get} and start again. *)
