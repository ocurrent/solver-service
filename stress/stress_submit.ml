open Lwt.Infix
open Current.Syntax

let ( >>!= ) = Lwt_result.bind

let solve_to_custom req builder =
  let params =
    Yojson.Safe.to_string
    @@ Solver_service_api.Worker.Solve_request.to_yojson req
  in
  let builder =
    Solver_service_api.Raw.Builder.Solver.Solve.Params.init_pointer builder
  in
  Solver_service_api.Raw.Builder.Solver.Solve.Params.request_set builder params

let remote_solve cluster job request =
  let action =
    Cluster_api.Submission.custom_build
    @@ Cluster_api.Custom.v ~kind:"solve"
    @@ solve_to_custom request
  in
  let build_pool =
    Current_ocluster.Connection.pool ~job ~pool:"solver" ~action ~cache_hint:""
      cluster
  in
  Current.Job.start_with ~pool:build_pool job ~level:Current.Level.Average
  >>= fun build_job ->
  Capnp_rpc_lwt.Capability.with_ref build_job
    (Current_ocluster.Connection.run_job ~job)

let ocaml_package_name = "ocaml-base-compiler"
let ocaml_version = "4.14.0"
let opam_repository_commit = "a9fb5a379794b0d5d7f663ff3a3bed5d4672a5d3"

let make_request vars opam_package =
  let package, opamfile = opam_package in
  Solver_service_api.Worker.Solve_request.
    {
      opam_repository_commits =
        [
          ( "https://github.com/ocaml/opam-repository.git",
            opam_repository_commit );
        ];
      root_pkgs = [ (String.cat package ".opam", opamfile) ];
      pinned_pkgs = [];
      platforms = [ ("macOS", vars); ("linux", vars); ("windows", vars) ];
      lower_bound = false;
    }

module Packages = struct
  open Solver_service_api

  let id = "opam-packages"

  type t = No_context

  module Key = Current.String

  module Value = struct
    type t = int [@@deriving yojson]

    let digest t = string_of_int t
  end

  module Outcome = struct
    type t = {
      packages : (string * string) list;
          (** package name and opam file content*)
      vars : Worker.Vars.t;
    }
    [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
      match Yojson.Safe.from_string s |> of_yojson with
      | Ok x -> x
      | Error e -> failwith e
  end

  let pp f (_, v) = Fmt.pf f "opam-packages: %s" (Value.digest v)

  let run No_context job _key limit =
    let open Lwt.Syntax in
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Utils.get_vars ~ocaml_package_name ~ocaml_version () >>= fun vars ->
    Utils.get_opam_packages () >>= fun pkgs ->
    pkgs
    |> List.filteri (fun n _ -> if n < limit + 1 then true else false)
    |> Lwt_list.mapi_s (fun id pkg ->
           (* use sequential map to avoid crashing if there's a log of packages (like 100 or 200),
            * this is because of get_opam_file which spawn a process*)
           let* opamfile = Utils.get_opam_file pkg in
           Current.Job.log job "package %d: %s@." id pkg;
           Lwt.return (pkg, opamfile))
    >>= fun packages -> Lwt_result.return { Outcome.packages; vars }

  let auto_cancel = true
  let latched = true
end

module Analysis = struct
  open Solver_service_api

  let id = "analysis"

  type t = Current_ocluster.Connection.t

  module Key = Current.String

  module Value = struct
    type t = Worker.Solve_request.t

    let digest t = Worker.Solve_request.to_yojson t |> Yojson.Safe.to_string
  end

  module Outcome = struct
    type t = Worker.Solve_response.t

    let marshal t = Worker.Solve_response.to_yojson t |> Yojson.Safe.to_string

    let unmarshal t =
      match Yojson.Safe.from_string t |> Worker.Solve_response.of_yojson with
      | Ok x -> x
      | Error e -> failwith e
  end

  let pp f (k, v) = Fmt.pf f "Analyse %a %s" Key.pp k (Value.digest v)

  let run t job _key value =
    remote_solve t job value >>!= fun response ->
    match
      Worker.Solve_response.of_yojson (Yojson.Safe.from_string response)
    with
    | Ok x -> Lwt_result.return x
    | Error ex -> failwith ex

  let auto_cancel = true
  let latched = true
end

module Analysis_current = Current_cache.Generic (Analysis)
module Packages_current = Current_cache.Generic (Packages)

let opam_packages limit =
  Current.component "Opam-packages: %d" limit
  |> let> () = Current.return () in
     Packages_current.run Packages.No_context
       (String.cat "opam-repository." opam_repository_commit)
       limit

let analyze cluster request package =
  Current.component "%s" package
  |> let> () = Current.return () in
     Analysis_current.run cluster package request

let pipeline cluster limit () =
  Current.component "Analysis"
  |> let** packages = opam_packages limit in
     let vars = packages.vars in
     packages.packages
     |> List.map (fun opam_pkg ->
            Current.ignore_value
            @@ analyze cluster (make_request vars opam_pkg) (fst opam_pkg))
     |> Current.all

let main config mode submission_uri limit =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat submission_uri in
  let cluster = Current_ocluster.Connection.create submission_cap in
  let engine = Current.Engine.create ~config (pipeline cluster limit) in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:"submit-stress-analysis"
      (Current_web.routes engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

open Cmdliner

let submission_service =
  Arg.required
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The submission.cap file for the build scheduler service"
       ~docv:"FILE" [ "submission-service" ]

let request_limit =
  Arg.value
  @@ Arg.opt Arg.int 30
  @@ Arg.info ~doc:"The number of requests to send" ~docv:"N" [ "limit" ]

let cmd =
  let doc = "Submit solve jobs to a scheduler that handles a solver-worker" in
  let info = Cmd.info "stress_remote" ~doc in
  Cmd.v info
    Term.(
      term_result
        (const main
        $ Current.Config.cmdliner
        $ Current_web.cmdliner
        $ submission_service
        $ request_limit))

let () = Cmd.(exit @@ eval cmd)
