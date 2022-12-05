open Solver_service
open Lwt.Syntax
module Service = Service.Make (Opam_repository)

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let reporter =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stderr
      ("%a %a %a @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      Fmt.(styled `Magenta string)
      (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  (* Disable tls.tracing when logs are set to debug *)
  (* List.iter
     (fun src -> match Logs.Src.name src with "tls.tracing" -> Logs.Src.set_level src (Some Info) | _ -> ())
     @@ Logs.Src.list (); *)
  Logs.set_reporter reporter;
  ()

let export service ~on:socket =
  let restore =
    Capnp_rpc_net.Restorer.single
      (Capnp_rpc_net.Restorer.Id.public "solver")
      service
  in
  let switch = Lwt_switch.create () in
  let stdin =
    Capnp_rpc_unix.Unix_flow.connect socket
    |> Capnp_rpc_net.Endpoint.of_flow
         (module Capnp_rpc_unix.Unix_flow)
         ~peer_id:Capnp_rpc_net.Auth.Digest.insecure ~switch
  in
  let (_ : Capnp_rpc_unix.CapTP.t) =
    Capnp_rpc_unix.CapTP.connect ~restore stdin
  in
  let crashed, set_crashed = Lwt.wait () in
  let* () =
    Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
        Lwt.wakeup_exn set_crashed (Failure "Capnp switch turned off");
        Lwt.return_unit)
  in
  crashed

let start_server address ~n_workers =
  let config =
    Capnp_rpc_unix.Vat_config.create ~secret_key:`Ephemeral address
  in
  let service_id =
    Capnp_rpc_unix.Vat_config.derived_id config "solver-service"
  in
  let create_worker commits =
    let cmd =
      ("", [| Sys.argv.(0); "--worker"; Remote_commit.list_to_string commits |])
    in
    Lwt_process.open_process cmd
  in
  let* service = Service.v ~n_workers ~create_worker in
  let restore = Capnp_rpc_net.Restorer.single service_id service in
  let+ vat = Capnp_rpc_unix.serve config ~restore in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let main () hash address n_workers =
  match (hash, address) with
  | None, Some address ->
      (* Run with a capnp address as the endpoint *)
      Lwt_main.run
        (let* uri = start_server address ~n_workers in
         Fmt.pr "Solver service running at: %a@." Uri.pp_hum uri;
         fst @@ Lwt.wait ())
  | None, None ->
      (* Run locally reading from stdin *)
      Lwt_main.run
        (let create_worker commits =
           let cmd =
             ( "",
               [|
                 Sys.argv.(0); "--worker"; Remote_commit.list_to_string commits;
               |] )
           in
           Lwt_process.open_process cmd
         in
         let* service = Service.v ~n_workers ~create_worker in
         export service ~on:Lwt_unix.stdin)
  | Some commits_str, _ ->
      Solver.main (Remote_commit.list_of_string_or_fail commits_str)

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let worker_commits =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The hash commits of the worker." ~docv:"COMMITS"
       [ "worker" ]

let internal_workers =
  Arg.value
  @@ Arg.opt Arg.int 20
  @@ Arg.info
       ~doc:"The number of threads that can handle more requests in parallel"
       ~docv:"N"
       [ "internal-thread-workers" ]

let address =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.Network.Location.cmdliner_conv) None
  @@ Arg.info
       ~doc:"The address to read requests from, if not provided will use stdin."
       ~docv:"ADDRESS" [ "address" ]

let cmd =
  let doc = "Solver for ocaml-ci" in
  let info = Cmd.info "solver-service" ~doc in
  Cmd.v info
    Term.(const main $ setup_log $ worker_commits $ address $ internal_workers)

let () = Cmd.(exit @@ eval cmd)
