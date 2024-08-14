(* This stress test is for the underlying solver-service library
   that the workers use to solve dependencies. *)

open Eio.Std

let ocaml_package_name = "ocaml-base-compiler"
let ocaml_version = "5.0.0"
let opam_repository_commit = "7109cb085f1064ae0722bdbdd0edf2d51bbe603b"

let linux_vars =
  { Solver_service_api.Worker.Vars.
    arch = "x86_64";
    os = "linux";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "12";
    ocaml_package = ocaml_package_name;
    ocaml_version;
    opam_version = "2.1.3";
    lower_bound = false;
  }

let windows_vars = (* (just guessing here) *)
  { Solver_service_api.Worker.Vars.
    arch = "x86_64";
    os = "win32";
    os_family = "windows";
    os_distribution = "windows";
    os_version = "11";
    ocaml_package = ocaml_package_name;
    ocaml_version;
    opam_version = "2.2.0";
    lower_bound = true;
  }

module Config = struct
  type t = {
    requests : Solver_service_api.Worker.Solve_request.t list;
    count : int;
    quiet : bool;
  }

  let compilers =
    [
      "4.01.0";
      "4.02.0";
      "4.03.0";
      "4.04.0";
      "4.05.0";
      "4.06.0";
      "4.07.0";
      "4.08.0";
      "4.09.0";
      "4.10.0";
      "4.11.0";
      "4.12.0";
      "4.13.0";
      "4.14.0";
    ] |> List.map (fun v ->
        v, { linux_vars with ocaml_version = v }
      )

  let platforms = [
    "linux", linux_vars;
    "windows", windows_vars;
  ] @ compilers

  let make_request ~test_packages package =
    let ( / ) = Eio.Path.( / ) in
    let opamfile = Eio.Path.load (test_packages / (package ^ ".opam")) in
    Solver_service_api.Worker.Solve_request.
      {
        opam_repository_commits =
          [ ("https://github.com/ocaml/opam-repository.git", opam_repository_commit) ];
        root_pkgs = [ (package, opamfile) ];
        pinned_pkgs = [];
        platforms;
      }

  let create ~test_packages count quiet =
    let packages =
      Eio.Path.read_dir test_packages
      |> List.to_seq
      |> Seq.filter_map (Filename.chop_suffix_opt ~suffix:".opam")
      |> Seq.take count
      |> List.of_seq
    in
    let requests = Fiber.List.map ~max_fibers:4 (make_request ~test_packages) packages in
    { requests; count; quiet }

  open Cmdliner

  let count =
    Arg.value
    @@ Arg.opt Arg.int 3
    @@ Arg.info ~doc:"The number of requests to send" ~docv:"N" [ "count" ]

  let quiet =
    Arg.value
    @@ Arg.flag
    @@ Arg.info ~doc:"Just output the rate" [ "q"; "quiet" ]

  let term ~test_packages =
    let make count = create ~test_packages count in
    Term.(const make $ count $ quiet)
end

let pp_request f (x : Solver_service_api.Worker.Solve_request.t) =
  Fmt.(Dump.list (using fst string)) f x.root_pkgs

let expected_failures_on_windows = ["core_unix.v0.16.0"; "inotify.2.4.1"]

let sel_id (sel : Solver_service_api.Worker.Selection.t) = sel.id

let benchmark (config:Config.t) solve =
  (* Warm-up *)
  let before = Unix.gettimeofday () in
  let results =
    config.requests
    |> Fiber.List.map (fun (req : Solver_service_api.Worker.Solve_request.t) ->
        let resp = solve req in
        if not config.quiet then (
          Format.printf "%a@."
            (Yojson.Safe.pretty_print ?std:None)
            (Solver_service_api.Worker.Solve_response.to_yojson resp);
        );
        begin
          match resp with
          | Error _ ->
            Fmt.failwith "Warm-up solve failed for %a" pp_request req
          | Ok sels ->
            let expected_failure = List.mem (fst (List.hd req.root_pkgs)) expected_failures_on_windows in
            let failure = not @@ List.exists (fun sel -> sel_id sel = "windows") sels in
            if expected_failure && not failure then
              Fmt.failwith "Warm-up solve should have failed for %a on Windows" pp_request req;
            if failure && not expected_failure then
              Fmt.failwith "Warm-up solve failed for %a on Windows" pp_request req
        end;
        (req, resp)
      )
  in
  let time = Unix.gettimeofday () -. before in
  if not config.quiet then Format.printf "@.Solved warm-up requests in: %.2fs@." time;
  let expected =
    results
    |> List.to_seq
    |> Seq.cycle
    |> Seq.take config.count
  in
  (* Main run *)
  if not config.quiet then Format.printf "Running another %d solves...@." config.count;
  let before = Unix.gettimeofday () in
  let complete = ref 0 in
  Switch.run (fun sw ->
      expected |> Seq.iter (fun (req, expected) ->
          Fiber.fork ~sw (fun () ->
              let resp = solve req in
              assert (resp = expected);
              incr complete;
              if not config.quiet then Printf.printf "\r%d/%d complete%!" !complete config.count
            )
        )
    );
  let time = Unix.gettimeofday () -. before in
  let rate = float (config.count * List.length Config.platforms) /. time in
  if config.quiet then (
    Format.printf "%.2f@." rate
  ) else (
    Format.printf "@.Solved %d requests in %.2fs (%.2fs/iter) (%.2f solves/s)@."
      config.count time (time /. float config.count)
      rate
  )

let verbose = false

let log =
  let module L = Solver_service_api.Raw.Service.Log in
  L.local @@ object
    inherit L.service

    method write_impl params release_param_caps =
      let open L.Write in
      release_param_caps ();
      let msg = Params.msg_get params in
      if verbose then output_string stderr msg;
      Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
  end

module Stress_local = struct
  type config = {
    cache_dir : string;
    n_workers : int;
  }

  let main ~domain_mgr ~process_mgr local config =
    Switch.run @@ fun sw ->
    let solver =
      Solver_service.Solver.create ~sw
        ~domain_mgr
        ~process_mgr
        ~cache_dir:local.cache_dir
        ~n_workers:local.n_workers
    in
    let service = Solver_service.Service.v solver in
    benchmark config (fun request ->
        Lwt_eio.run_lwt @@ fun () ->
        Solver_service_api.Solver.solve service ~log request
      )

  open Cmdliner

  let internal_workers =
    Arg.value
    @@ Arg.opt Arg.int (Domain.recommended_domain_count () - 1)
    @@ Arg.info ~doc:"The number of sub-process solving requests in parallel"
      ~docv:"N" [ "internal-workers" ]

  let cache_dir =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"Path cached Git clones" ~docv:"DIR"
      [ "cache-dir" ]

  let config =
    let make cache_dir n_workers = { cache_dir; n_workers } in
    Term.(const make $ cache_dir $ internal_workers)
end

module Stress_service = struct
  let verbose = false

  let log =
    let module L = Solver_service_api.Raw.Service.Log in
    L.local @@ object
      inherit L.service

      method write_impl params release_param_caps =
        let open L.Write in
        release_param_caps ();
        let msg = Params.msg_get params in
        if verbose then output_string stderr msg;
        Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
    end

  let main vat uri config =
    let sr = Capnp_rpc_unix.Vat.import_exn vat uri in
    Lwt_eio.run_lwt @@ fun () ->
    Capnp_rpc_unix.with_cap_exn sr @@ fun solver ->
    Lwt_eio.run_eio @@ fun () ->
    benchmark config (fun request ->
        Lwt_eio.run_lwt @@ fun () ->
        Solver_service_api.Solver.solve solver ~log request
      )
end

module Stress_cluster = struct
  open Lwt.Infix
  open Capnp_rpc_lwt

  module Worker = Solver_service_api.Worker

  let solve_to_custom req builder =
    let params = Yojson.Safe.to_string @@ Worker.Solve_request.to_yojson req in
    let module Params = Solver_service_api.Raw.Builder.Solver.Solve.Params in
    let builder = Params.init_pointer builder in
    Params.request_set builder params

  let solve_with_cluster ~pool sched request =
    let action =
      Cluster_api.Submission.custom_build
      @@ Cluster_api.Custom.v ~kind:"solve"
      @@ solve_to_custom request
    in
    Lwt_eio.run_lwt @@ fun () ->
    Capability.with_ref (Cluster_api.Submission.submit sched
                           ~action
                           ~urgent:false
                           ~pool
                           ~cache_hint:"") @@ fun ticket ->
    Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun build_job ->
    Cluster_api.Job.result build_job >>= function
    | Error (`Capnp e) -> Fmt.failwith "%a" Capnp_rpc.Error.pp e
    | Ok response ->
      match Worker.Solve_response.of_yojson (Yojson.Safe.from_string response) with
      | Error e -> failwith e
      | Ok x -> Lwt.return x

  let main vat uri pool config =
    let sr = Capnp_rpc_unix.Vat.import_exn vat uri in
    Lwt_eio.run_lwt @@ fun () ->
    Capnp_rpc_unix.with_cap_exn sr @@ fun sched ->
    Lwt_eio.run_eio @@ fun () ->
    benchmark config (solve_with_cluster ~pool sched)
end

open Cmdliner

let solver_service =
  Arg.required
  @@ Arg.pos 0 Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The solver.cap file for the solver service"
    ~docv:"FILE" []

let submission_service =
  Arg.required
  @@ Arg.pos 0 Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The submission.cap file for the build scheduler service"
    ~docv:"FILE" []

let solver_pool =
  Arg.value
  @@ Arg.opt Arg.string "solver"
  @@ Arg.info ~doc:"The OCluster pool to use for solves"
    ~docv:"POOL" ["solver-pool"]

let () =
  exit @@ Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun () ->
  let ( / ) = Eio.Path.( / ) in
  let test_packages = env#cwd / "stress/test-packages" in
  let vat = Capnp_rpc_unix.client_only_vat () in
  let stress_service vat =
    let doc = "Submit solve jobs to a solver service" in
    let info = Cmd.info "service" ~doc in
    Cmd.v info Term.(const (Stress_service.main vat) $ solver_service $ Config.term ~test_packages)
  in
  let stress_cluster vat =
    let doc = "Submit solve jobs to a scheduler that handles a solver-worker" in
    let info = Cmd.info "cluster" ~doc in
    Cmd.v info Term.(const (Stress_cluster.main vat) $ submission_service $ solver_pool $ Config.term ~test_packages)
  in
  let stress_local =
    let doc = "Run jobs using an in-process solver" in
    let info = Cmd.info "local" ~doc in
    let domain_mgr = env#domain_mgr in
    let process_mgr = env#process_mgr in
    Cmd.v info Term.(const (Stress_local.main ~domain_mgr ~process_mgr) $ Stress_local.config $ Config.term ~test_packages)
  in
  let doc = "stress test the solver" in
  let info = Cmd.info "stress" ~doc in
  Cmd.eval @@ Cmd.group info [ stress_local; stress_service vat; stress_cluster vat ]
