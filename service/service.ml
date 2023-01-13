open Lwt.Infix
open Capnp_rpc_lwt
module Worker = Solver_service_api.Worker
module Log = Solver_service_api.Solver.Log
module Selection = Worker.Selection
module Store = Git_unix.Store

module Make (Opam_repo : Opam_repository_intf.S) = struct
  module Epoch : sig
    type t
    (* An Epoch handles all requests for a single opam-repository HEAD commit. *)

    val create :
      n_workers:int ->
      create_worker:(Remote_commit.t list -> Lwt_process.process) ->
      Remote_commit.t list ->
      t Lwt.t

    val process :
      log:Log.X.t Capability.t ->
      id:string ->
      Worker.Solve_request.t ->
      < stdin : Lwt_io.output_channel ; stdout : Lwt_io.input_channel ; .. > ->
      (string list, string) result Lwt.t

    val handle :
      log:Solver_service_api.Solver.Log.t ->
      Worker.Solve_request.t ->
      t ->
      Selection.t list Lwt.t

    val dispose : t -> unit Lwt.t
  end = struct
    type t = Lwt_process.process Lwt_pool.t

    let validate (worker : Lwt_process.process) =
      match Lwt.state worker#status with
      | Lwt.Sleep -> Lwt.return true
      | Lwt.Fail ex -> Lwt.fail ex
      | Lwt.Return status ->
          Format.eprintf "Worker %d is dead (%a) - removing from pool@."
            worker#pid Process.pp_status status;
          Lwt.return false

    let dispose (worker : Lwt_process.process) =
      let pid = worker#pid in
      Fmt.epr "Terminating worker %d@." pid;
      worker#terminate;
      worker#status >|= fun _ -> Fmt.epr "Worker %d finished@." pid

    let update_opam_repository_to_commit commit =
      let repo_url = commit.Remote_commit.repo in
      let hash = Store.Hash.of_hex commit.Remote_commit.hash in
      Opam_repo.open_store ~repo_url () >>= fun store ->
      Store.mem store hash >>= function
      | true -> Lwt.return_unit
      | false -> (
          Fmt.pr "Need to update %s to get new commit %a@." repo_url
            Store.Hash.pp hash;
          Opam_repo.fetch ~repo_url () >>= fun () ->
          Opam_repo.open_store ~repo_url () >>= fun new_store ->
          Store.mem new_store hash >>= function
          | false -> Fmt.failwith "Still missing commit after update!"
          | true -> Lwt.return_unit)

    let create ~n_workers ~create_worker commits =
      Lwt_list.iter_p update_opam_repository_to_commit commits >|= fun () ->
      Lwt_pool.create n_workers ~validate ~dispose (fun () ->
          Lwt.return (create_worker commits))

    let dispose = Lwt_pool.clear

    (* Send [request] to [worker] and read the reply. *)
    let process ~log ~id request worker =
      let request_str =
        Worker.Solve_request.to_yojson request |> Yojson.Safe.to_string
      in
      let request_str =
        Printf.sprintf "%d\n%s" (String.length request_str) request_str
      in
      Lwt_io.write worker#stdin request_str >>= fun () ->
      Lwt_io.read_line worker#stdout >>= fun time ->
      Lwt_io.read_line worker#stdout >>= fun len ->
      match Astring.String.to_int len with
      | None -> Fmt.failwith "Bad frame from worker: time=%S len=%S" time len
      | Some len -> (
          let buf = Bytes.create len in
          Lwt_io.read_into_exactly worker#stdout buf 0 len >|= fun () ->
          let results = Bytes.unsafe_to_string buf in
          match results.[0] with
          | '+' ->
              Log.info log "%s: found solution in %s s" id time;
              let packages =
                Astring.String.with_range ~first:1 results
                |> Astring.String.cuts ~sep:" "
              in
              Ok packages
          | '-' ->
              Log.info log "%s: eliminated all possibilities in %s s" id time;
              let msg = results |> Astring.String.with_range ~first:1 in
              Error msg
          | '!' ->
              let msg = results |> Astring.String.with_range ~first:1 in
              Fmt.failwith "BUG: solver worker failed: %s" msg
          | _ -> Fmt.failwith "BUG: bad output: %s" results)

    let ocaml = OpamPackage.Name.of_string "ocaml"
    let base_effects = OpamPackage.Name.of_string "base-effects"
    let base_domains = OpamPackage.Name.of_string "base-domains"

    let is_multicore ocaml_version =
      let v =
        Ocaml_version.of_string_exn
          (OpamPackage.Version.to_string ocaml_version)
      in
      Ocaml_version.Configure_options.is_multicore v

    (* If a local package has a literal constraint on OCaml's version and it doesn't match
       the platform, we just remove that package from the set to test, so other packages
       can still be tested. If it depends on base-effects or base-domains, require a multicore compiler. *)
    let compatible_with ~log ~ocaml_version (dep_name, filter) =
      let check_ocaml = function
        | OpamTypes.Constraint (op, OpamTypes.FString v) ->
            let v = OpamPackage.Version.of_string v in
            OpamFormula.eval_relop op ocaml_version v
        | _ -> true
      in
      if OpamPackage.Name.equal dep_name ocaml then
        OpamFormula.eval check_ocaml filter
      else if
        OpamPackage.Name.equal dep_name base_effects
        || OpamPackage.Name.equal dep_name base_domains
      then (
        try is_multicore ocaml_version
        with ex ->
          Log.info log "is_multicore %S failed: %a"
            (OpamPackage.Version.to_string ocaml_version)
            Fmt.exn ex;
          false)
      else true

    let handle ~log request t =
      let {
        Worker.Solve_request.opam_repository_commits;
        platforms;
        root_pkgs;
        pinned_pkgs;
        prefer_oldest = _prefer_oldest;
      } =
        request
      in
      let root_pkgs = List.map fst root_pkgs in
      let pinned_pkgs = List.map fst pinned_pkgs in
      let pins =
        root_pkgs @ pinned_pkgs
        |> List.map (fun pkg -> OpamPackage.name (OpamPackage.of_string pkg))
        |> OpamPackage.Name.Set.of_list
      in
      Log.info log "Solving for %a" Fmt.(list ~sep:comma string) root_pkgs;
      platforms
      |> Lwt_list.map_p (fun p ->
             let id, vars = p in
             let ocaml_version =
               OpamPackage.Version.of_string vars.Worker.Vars.ocaml_version
             in
             let compatible_root_pkgs =
               request.root_pkgs
               |> List.filter (fun (_name, contents) ->
                      if String.equal "" contents then true
                      else
                        let opam = OpamFile.OPAM.read_from_string contents in
                        let deps = OpamFile.OPAM.depends opam in
                        OpamFormula.eval
                          (compatible_with ~log ~ocaml_version)
                          deps)
             in
             (* If some packages are compatible but some aren't, just solve for the compatible ones.
                Otherwise, try to solve for everything to get a suitable error. *)
             let root_pkgs =
               if compatible_root_pkgs = [] then request.root_pkgs
               else compatible_root_pkgs
             in
             let slice = { request with platforms = [ p ]; root_pkgs } in
             Lwt_pool.use t (process ~log ~id slice) >>= function
             | Error _ as e -> Lwt.return (id, e)
             | Ok packages ->
                 let repo_packages =
                   packages
                   |> List.filter_map (fun pkg ->
                          let pkg = OpamPackage.of_string pkg in
                          if OpamPackage.Name.Set.mem pkg.name pins then None
                          else Some pkg)
                 in
                 (* Hack: ocaml-ci sometimes also installs odoc, but doesn't tell us about it.
                    Make sure we have at least odoc 2.1.1 available, otherwise it won't work on OCaml 5.0. *)
                 let repo_packages =
                   OpamPackage.of_string "odoc.2.1.1" :: repo_packages
                 in
                 Opam_repo.oldest_commits_with repo_packages
                   ~from:opam_repository_commits
                 >|= fun commits ->
                 let compat_pkgs = List.map fst compatible_root_pkgs in
                 (id, Ok { Worker.Selection.id; compat_pkgs; packages; commits }))
      >|= List.filter_map (fun (id, result) ->
              Log.info log "= %s =" id;
              match result with
              | Ok result ->
                  Log.info log "-> @[<hov>%a@]"
                    Fmt.(list ~sep:sp string)
                    result.Selection.packages;
                  Log.info log "(valid since opam-repository commit(s) :@[%a@])"
                    Fmt.(list ~sep:semi (pair ~sep:comma string string))
                    result.Selection.commits;
                  Some result
              | Error msg ->
                  Log.info log "%s" msg;
                  None)
  end

  (* Handle a request by distributing it among the worker processes and then aggregating their responses. *)
  let handle t ~log (request : Worker.Solve_request.t) =
    let commits =
      request.opam_repository_commits
      |> List.map (fun (repo, hash) -> Remote_commit.v ~repo ~hash)
    in
    Epoch_lock.with_epoch t commits (Epoch.handle ~log request)

  let v ~n_workers ~create_worker =
    let create commits = Epoch.create ~n_workers ~create_worker commits in
    let t = Epoch_lock.v ~create ~dispose:Epoch.dispose () in
    let module X = Solver_service_api.Raw.Service.Solver in
    Lwt.return
    @@ X.local
    @@ object
         inherit X.service

         method solve_impl params release_param_caps =
           let open X.Solve in
           let request = Params.request_get params in
           let log = Params.log_get params in
           release_param_caps ();
           match log with
           | None -> Service.fail "Missing log argument!"
           | Some log -> (
               Capnp_rpc_lwt.Service.return_lwt @@ fun () ->
               Capability.with_ref log @@ fun log ->
               match
                 Worker.Solve_request.of_yojson
                   (Yojson.Safe.from_string request)
               with
               | Error msg ->
                   Lwt_result.fail
                     (`Capnp
                       (Capnp_rpc.Error.exn "Bad JSON in request: %s" msg))
               | Ok request ->
                   Lwt.catch
                     (fun () -> handle t ~log request >|= Result.ok)
                     (function
                       | Failure msg -> Lwt_result.fail (`Msg msg)
                       | ex -> Lwt.return (Fmt.error_msg "%a" Fmt.exn ex))
                   >|= fun selections ->
                   let json =
                     Yojson.Safe.to_string
                       (Worker.Solve_response.to_yojson selections)
                   in
                   let response, results =
                     Capnp_rpc_lwt.Service.Response.create Results.init_pointer
                   in
                   Results.response_set results json;
                   Ok response)
       end
end
