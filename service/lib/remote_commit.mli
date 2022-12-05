(** A description of a remote repository used for opam repositories *)

type t = {
  repo : string; (* The url of the repo from which to pull *)
  hash : string; (* Hash of the branch master *)
}
[@@deriving yojson]

val v : repo:string -> hash:string -> t
val repo : t -> string
val hash : t -> string
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val of_string : string -> (t, string) result
val list_to_string : t list -> string
val of_yojson_or_fail : Yojson.Safe.t -> t
val list_of_string_or_fail : string -> t list
