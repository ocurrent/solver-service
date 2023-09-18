(** Manages a collection of local Git clones.

    This is only used from the main domain. *)

type commit = string * string
(** A [(repository_url, commit_hash)] pair,
    identifying a specific commit and a remote from which it can be fetched. *)

type t

val create : process_mgr:_ Eio.Process.mgr -> cache_dir:string -> t
(** [create ~process_mgr ~cache_dir] is a local Git cache that maintains local Git clones
    in [cache_dir]. It uses [process_mgr] to run the git command. *)

val fetch_commits : t -> commit list -> (unit, [> `Msg of string]) result
(** [fetch_commits t commits] checks that each commit is present and fetches from the remote if not. *)

val packages : t -> commit list -> (Packages.t, [> `Msg of string]) result
(** [packages t commits] indexes the union of [commits]. *)

val oldest_commits_with : t -> from:commit list -> OpamPackage.t list -> commit list
(** [oldest_commits_with t ~from packages] finds the oldest commits whose union has the same
    package information as [from] (for [packages]). This is useful to avoid unnecessary rebuilds. *)
