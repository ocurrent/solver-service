(** This is like [Stdlib.Lazy], but multiple domains can force it at once. *)

type 'a t

val make : (unit -> 'a) -> 'a t
(** [make fn] is a lazy value that runs [fn ()] the first time it is forced. *)

val force : 'a t -> 'a
(** [force t] atomically ensures the function passed to [make] has been started
    and then awaits the promise of the result. *)
