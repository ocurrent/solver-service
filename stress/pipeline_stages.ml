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

let make_request opam_package opam_repository_commit =
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
      platforms = [];
      lower_bound = false;
    }

module Packages = struct
  let id = "opam-packages"
  let pool = Lwt_pool.create 10 (fun () -> Lwt.return_unit)

  type t = No_context

  module Key = Current.String

  module Value = struct
    type t = int [@@deriving yojson]

    let digest t = string_of_int t
  end

  module Outcome = struct
    type t = (string * string) list [@@deriving yojson]
    (** package name and opam file content*)

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
    Utils.get_opam_packages () >>= fun pkgs ->
    pkgs
    |> List.filteri (fun n _ -> if n < limit + 1 then true else false)
    |> List.mapi (fun id pkg ->
           (* use sequential map to avoid crashing if there's a log of packages (like 100 or 200),
               * this is because of get_opam_file which spawn a process*)
           Lwt_pool.use pool @@ fun () ->
           let* opamfile = Utils.get_opam_file pkg in
           Current.Job.log job "package %d: %s@." id pkg;
           Lwt.return (pkg, opamfile))
    |> Lwt_list.map_s (fun p -> p)
    >>= Lwt_result.return

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
    type t = Worker.Selection.t list [@@deriving yojson]

    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
      match Yojson.Safe.from_string s |> of_yojson with
      | Ok x -> x
      | Error e -> failwith e
  end

  let pp f (k, v) = Fmt.pf f "Analyse %a %s" Key.pp k (Value.digest v)

  let run t job _key value =
    remote_solve t job value >>!= fun response ->
    match
      Worker.Solve_response.of_yojson (Yojson.Safe.from_string response)
    with
    | Ok x -> Lwt.return x
    | Error ex -> failwith ex

  let auto_cancel = true
  let latched = true
end

module Opam_hash = struct
  type t = No_context

  let current_hash = ref (-1)

  module Key = struct
    type t = string list [@@deriving yojson]

    let digest t = to_yojson t |> Yojson.Safe.to_string
  end

  module Value = Current.String

  let id = "opam-hash"

  let build No_context job key =
    let opam_repository_hash = key in
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    incr current_hash;
    if !current_hash >= List.length opam_repository_hash then current_hash := 0;
    let commit = List.nth opam_repository_hash !current_hash in
    Current.Job.log job "Current opam repository_commit: %s@." commit;
    Lwt.return @@ Ok commit

  let pp f key = Fmt.pf f "opam-hash: %s" (Key.digest key)
  let auto_cancel = false
end

module Platforms = struct
  let id = "platforms"

  type t = No_context

  module Key = Current.String
  (* module Value = Current.String *)

  module Value = struct
    type t = (string * Solver_service_api.Worker.Vars.t) list
    [@@deriving yojson]

    (* let digest t = to_yojson t |> Yojson.Safe.to_string *)
    let marshal t = to_yojson t |> Yojson.Safe.to_string

    let unmarshal s =
      match Yojson.Safe.from_string s |> of_yojson with
      | Ok x -> x
      | Error e -> failwith e
  end

  let pp f key = Fmt.pf f "platforms(%a):" Key.pp key

  let build No_context job _key =
    let ocaml_package_name = "ocaml-base-compiler" in
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Utils.get_vars ~ocaml_package_name ~ocaml_version:"none" () >>= fun vars ->
    let platforms =
      List.rev Ocaml_version.Releases.all_patches
      |> List.filteri (fun i _ -> i < 8)
      |> List.map (fun y ->
             let platform =
               (vars.os, { vars with ocaml_version = Ocaml_version.to_string y })
             in
             Current.Job.log job "%s@." (Value.marshal [ platform ]);
             platform)
    in
    Lwt_result.return platforms

  let auto_cancel = true
end

module Analysis_current = Current_cache.Generic (Analysis)
module Packages_current = Current_cache.Generic (Packages)
module Opam_hash_current = Current_cache.Make (Opam_hash)
module Platforms_current = Current_cache.Make (Platforms)

let platforms () =
  Current.component "Platforms"
  |> let> () = Current.return () in
     Platforms_current.get Platforms.No_context "platforms-current"

let opam_hash auto_cancel_time opam_repository_commits () =
  Current.component "Opam-hash"
  |> let> () = Current.return () in
     let schedule =
       (Current_cache.Schedule.v ~valid_for:(Duration.of_sec auto_cancel_time))
         ()
     in
     Opam_hash_current.get ~schedule Opam_hash.No_context
       opam_repository_commits

let opam_packages limit =
  Current.component "Opam-packages: %d" limit
  |> let> () = Current.return () in
     Packages_current.run Packages.No_context (string_of_int limit) limit

let analyze platforms opam_repository_commit cluster opam_pkg =
  Current.component "Analyse@.%s" (fst opam_pkg)
  |> let> () = Current.return () in
     let package = fst opam_pkg in
     let make_request opam_pkg =
       make_request opam_pkg opam_repository_commit |> fun req ->
       { req with platforms }
     in
     Analysis_current.run cluster package (make_request opam_pkg)
