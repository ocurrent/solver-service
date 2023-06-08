open Lwt.Infix

type error = [ `Cancelled | `Exit_code of int | `Msg of string ]

let send_to ch contents =
  Lwt.try_bind
    (fun () -> Lwt_io.write ch contents >>= fun () -> Lwt_io.close ch)
    (fun () -> Lwt_result.return ())
    (fun ex -> Lwt.return (Fmt.error_msg "%a" Fmt.exn ex))

let exec ~label ~log ~switch ?env ?(stdin = "") ?(stderr = `FD_copy Unix.stdout)
    ?(is_success = ( = ) 0) cmd =
  Log.info (fun f ->
      f "Exec(%s): %a" label Fmt.(list ~sep:sp (quote string)) cmd);
  let cmd = ("", Array.of_list cmd) in
  Lwt_process.with_process ?env ~stderr cmd @@ fun proc ->
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      if Lwt.state proc#status = Lwt.Sleep then (
        Log.info (fun f -> f "Cancelling %s job..." label);
        proc#terminate);
      Lwt.return_unit)
  >>= fun () ->
  let copy_thread = Log_data.copy_from_stream log proc#stdout in
  send_to proc#stdin stdin >>= fun stdin_result ->
  copy_thread >>= fun () ->
  (* Ensure all data has been copied before returning *)
  proc#status >|= function
  | _ when not (Lwt_switch.is_on switch) -> Error `Cancelled
  | Unix.WEXITED n when is_success n -> (
      match stdin_result with
      | Ok _ as ok -> ok
      | Error (`Msg msg) ->
          Fmt.error_msg "Failed sending input to %s: %s" label msg)
  | Unix.WEXITED n -> Error (`Exit_code n)
  | Unix.WSIGNALED x ->
      Fmt.error_msg "%s failed with signal %a" label Fmt.Dump.signal x
  | Unix.WSTOPPED x ->
      Fmt.error_msg "%s stopped with signal %a" label Fmt.Dump.signal x

let check_call ~label ~log ~switch ?env ?stdin ?stderr ?is_success cmd =
  exec ~label ~log ~switch ?env ?stdin ?stderr ?is_success cmd >|= function
  | Ok () -> Ok ()
  | Error `Cancelled -> Error `Cancelled
  | Error (`Exit_code n) -> Fmt.error_msg "%s failed with exit-code %d" label n
  | Error (`Msg _) as e -> e
