(** Helper functions for Lwt process handling. *)

open Lwt.Infix

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.(array ~sep (quote string))

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let pp_status f = function
  | Unix.WEXITED x -> Fmt.pf f "exited with status %d" x
  | Unix.WSIGNALED x -> Fmt.pf f "failed with signal %a" Fmt.Dump.signal x
  | Unix.WSTOPPED x -> Fmt.pf f "stopped with signal %a" Fmt.Dump.signal x

let check_status cmd = function
  | Unix.WEXITED 0 -> ()
  | status -> Fmt.failwith "%a %a" pp_cmd cmd pp_status status

let exec cmd =
  Lwt_process.with_process_none cmd @@ fun proc ->
  proc#status >|= check_status cmd

let pread cmd =
  Lwt_process.with_process_in cmd @@ fun proc ->
  Lwt_io.read proc#stdout >>= fun output ->
  proc#status >|= check_status cmd >|= fun () -> output
