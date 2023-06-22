open Lwt.Infix

module Solver_process = struct
  type state =
    | Available
    | Released
    | Closed of Unix.process_status
    | Failed of exn

  type t = { process : Lwt_process.process; mutable state : state }

  let create cmd = { process = Lwt_process.open_process cmd; state = Available }
  let pid t = t.process#pid

  let state t =
    match Lwt.state t.process#status with
    | Lwt.Sleep -> t.state
    | Lwt.Fail ex -> Failed ex
    | Lwt.Return status -> Closed status

  let release t = t.state <- Released

  let close t =
    release t;
    t.process#terminate;
    t.process#close >|= fun status ->
    t.state <- Closed status;
    status

  let read_line t = Lwt_io.read_line t.process#stdout
  let write_line t msg = Lwt_io.write_line t.process#stdin msg
  let write t msg = Lwt_io.write t.process#stdin msg

  let read_into t len =
    let buf = Bytes.create len in
    Lwt_io.read_into_exactly t.process#stdout buf 0 len >|= fun () ->
    Bytes.unsafe_to_string buf
end
