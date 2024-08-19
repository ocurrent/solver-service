module Cache : sig
  type t

  val create : unit -> t
end

val oldest_commit_with :
  cache:Cache.t ->
  store:Git_unix.Store.t ->
  from:Git_unix.Store.hash ->
  string list ->
  string option
(** [oldest_commit_with ~cache ~store ~from paths] returns a commit where
    [paths] have the same contents as in [from].

    Returns [None] if none of the paths are in [from].

    This is equivalent to doing "git log -1 --first-parent $from -- $paths". *)
