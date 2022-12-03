(* Workers that can also solve opam jobs *)

open Lwt.Syntax
module Log_data = Log_data
module Context = Context
module Log = Log
module Process = Process

let solve_to_custom req =
  let open Cluster_api.Raw in
  let params =
    Yojson.Safe.to_string
    @@ Solver_service_api.Worker.Solve_request.to_yojson req
  in
  let custom = Builder.Custom.init_root () in
  let builder = Builder.Custom.payload_get custom in
  let request =
    Solver_service_api.Raw.Builder.Solver.Solve.Params.init_pointer builder
  in
  Solver_service_api.Raw.Builder.Solver.Solve.Params.request_set request params;
  let r = Reader.Custom.of_builder custom in
  Reader.Custom.payload_get r

let solve_of_custom c =
  let open Solver_service_api.Raw in
  let payload = Cluster_api.Custom.payload c in
  let r = Reader.of_pointer payload in
  let request = Reader.Solver.Solve.Params.request_get r in
  Solver_service_api.Worker.Solve_request.of_yojson
  @@ Yojson.Safe.from_string request

module Service = Solver_service.Service.Make (Solver_service.Opam_repository)

let cluster_worker_log log =
  let module L = Solver_service_api.Raw.Service.Log in
  L.local
  @@ object
       inherit L.service

       method write_impl params release_param_caps =
         let open L.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Log_data.write log msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

let solve ~solver ~switch:_ ~log c =
  match solve_of_custom c with
  | Error m -> failwith m
  | Ok s ->
      let+ response =
        Solver_service_api.Solver.solve ~log:(cluster_worker_log log) solver s
      in
      let response =
        Yojson.Safe.to_string
        @@ Solver_service_api.Worker.Solve_response.to_yojson response
      in
      Log_data.write log response;
      Ok response

let temp_file_name prefix suffix =
  let temp_dir = Filename.get_temp_dir_name () in
  let rnd = Random.(State.bits (get_state ())) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let spawn_local ?solver_dir ~internal_workers () : Solver_service_api.Solver.t =
  let name = temp_file_name "solver-worker" ".sock" in
  Logs.info (fun f -> f "Setting up solver %s…" name);
  let listener = Unix.socket ~cloexec:true PF_UNIX SOCK_STREAM 0 in
  (try Unix.unlink name with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  Unix.bind listener (ADDR_UNIX name);
  Unix.listen listener 1;
  let solver_dir =
    match solver_dir with
    | None -> Fpath.to_string (Current.state_dir "solver")
    | Some x -> x
  in
  let cmd =
    ( "",
      [|
        "solver-service";
        "--internal-thread-workers";
        string_of_int internal_workers;
        "--sockpath";
        name;
      |] )
  in
  let _child =
    Lwt_process.open_process_none ~cwd:solver_dir ~stdin:`Close cmd
  in
  let switch = Lwt_switch.create () in
  let p, _ =
    match Unix.select [ listener ] [] [] 1. with
    | [ listener' ], [], [] when listener = listener' ->
        Unix.accept ~cloexec:true listener
    | _ -> failwith "Solver process didn't start correctly"
    | exception (Unix.Unix_error (Unix.EINTR, _, _) as exn) ->
        prerr_endline "Solver process didn't start correctly";
        raise exn
  in
  Unix.close listener;
  Unix.unlink name;
  let p =
    Lwt_unix.of_unix_file_descr p
    |> Capnp_rpc_unix.Unix_flow.connect ~switch
    |> Capnp_rpc_net.Endpoint.of_flow
         (module Capnp_rpc_unix.Unix_flow)
         ~peer_id:Capnp_rpc_net.Auth.Digest.insecure ~switch
  in
  let conn =
    Capnp_rpc_unix.CapTP.connect ~restore:Capnp_rpc_net.Restorer.none p
  in
  let solver =
    Capnp_rpc_unix.CapTP.bootstrap conn
      (Capnp_rpc_net.Restorer.Id.public "solver")
  in
  solver
  |> Capnp_rpc_lwt.Capability.when_broken (fun ex ->
         Fmt.failwith "Solver process failed: %a" Capnp_rpc.Exception.pp ex);
  solver
