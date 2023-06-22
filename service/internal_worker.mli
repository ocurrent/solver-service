module Solver_process : sig
  type state =
    | Available
    | Released
    | Closed of Unix.process_status
    | Failed of exn

  type t = { process : Lwt_process.process; mutable state : state }

  val create : Lwt_process.command -> t
  val pid : t -> int
  val state : t -> state
  val read_line : t -> String.t Lwt.t
  val write_line : t -> string -> unit Lwt.t
  val write : t -> string -> unit Lwt.t
  val read_into : t -> int -> string Lwt.t
  val release : t -> unit
  val close : t -> Unix.process_status Lwt.t
end
