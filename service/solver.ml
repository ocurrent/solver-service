open Eio.Std
module Worker = Solver_service_api.Worker
module Log = Solver_service_api.Solver.Log
module Selection = Worker.Selection

let (let*!) = Result.bind

type t = {
  pool : (Domain_worker.request, Domain_worker.reply) Pool.t;
  stores : Stores.t;
  cache_dir : string;
  process_mgr : [`Generic] Eio.Process.mgr_ty r;
}

let ocaml = OpamPackage.Name.of_string "ocaml"

(* If a local package has a literal constraint on OCaml's version and it doesn't match
   the platform, we just remove that package from the set to test, so other packages
   can still be tested. *)
let compatible_with ~ocaml_version (dep_name, filter) =
  let check_ocaml = function
    | OpamTypes.Constraint (op, OpamTypes.FString v) ->
        let v = OpamPackage.Version.of_string v in
        OpamFormula.eval_relop op ocaml_version v
    | _ -> true
  in
  if OpamPackage.Name.equal dep_name ocaml then
    OpamFormula.eval check_ocaml filter
  else true

let env vars v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else Domain_worker.env vars (OpamVariable.Full.to_string v)

let solve_for_platform ?cancelled t ~cacheable ~log ~opam_repository_commits ~packages ~root_pkgs ~pinned_pkgs ~pins ~vars id =
  let ocaml_version = OpamPackage.Version.of_string vars.Worker.Vars.ocaml_version in
  let root_pkgs =
    root_pkgs
    |> List.filter (fun (_name, (_version, opam)) ->
        let avail = OpamFile.OPAM.available opam in
        let deps = OpamFile.OPAM.depends opam in
        let env = env vars in
        OpamFilter.eval_to_bool ~default:true env avail &&
        OpamFormula.eval (compatible_with ~ocaml_version) deps)
  in
  if root_pkgs = [] then (
    Log.info log "%s: not available for this platform" id;
    Error (`No_solution "Not available for this platform")
  ) else (
    let slice = { Domain_worker.vars; root_pkgs; packages; pinned_pkgs; cancelled } in
    match Pool.use t.pool slice with
    | Error (`Msg m) -> Error (`Msg m)
    | Ok (results, time) ->
      match results with
      | Error e ->
        Log.info log "%s: eliminated all possibilities in %.2f s" id time;
        Error (`No_solution e)
      | Ok packages ->
        Log.info log "%s: found solution in %.2f s" id time;
        let commits =
          if cacheable then
            (* The cache system handle a sort of oldest_commit*)
            opam_repository_commits
          else
            let repo_packages =
              packages
              |> List.filter_map (fun (pkg : OpamPackage.t) ->
                if OpamPackage.Name.Set.mem pkg.name pins then None
                else Some pkg)
            in
            (* Hack: ocaml-ci sometimes also installs odoc, but doesn't tell us about it.
           Make sure we have at least odoc 2.1.1 available, otherwise it won't work on OCaml 5.0. *)
            let repo_packages =
              OpamPackage.of_string "odoc.2.1.1" :: repo_packages
            in
            let commits = Stores.oldest_commits_with t.stores repo_packages ~from:opam_repository_commits in
            commits
        in
        let compat_pkgs =
          let to_string (name, (version,_)) = OpamPackage.to_string (OpamPackage.create name version) in
          List.map to_string root_pkgs
        in
        let packages = List.map OpamPackage.to_string packages in
        let lower_bound = vars.lower_bound in
        Ok { Worker.Selection.id; compat_pkgs; packages; commits; lower_bound }
  )

let pp_exn f = function
  | Failure msg -> Fmt.string f msg
  | ex -> Eio.Exn.pp f ex

let parse_opam (name, contents) =
  try
    let pkg = OpamPackage.of_string name in
    let opam = OpamFile.OPAM.read_from_string contents in
    Ok (OpamPackage.name pkg, (OpamPackage.version pkg, opam))
  with ex ->
    Fmt.error_msg "Error parsing %s: %a" name pp_exn ex

let rec parse_opams = function
  | [] -> Ok []
  | x :: xs ->
    let*! x = parse_opam x in
    let*! xs = parse_opams xs in
    Ok (x :: xs)

(* Handle a request by distributing it among the worker processes and then aggregating their responses. *)
let solve ?cancelled ~cacheable t ~log request =
  let {
    Worker.Solve_request.opam_repository_commits;
    platforms;
    root_pkgs;
    pinned_pkgs;
  } =
    request
  in
  let*! () = Stores.fetch_commits t.stores opam_repository_commits in
  let root_pkgs = List.map fst root_pkgs in
  let pinned_pkgs = List.map fst pinned_pkgs in
  let pins =
    root_pkgs @ pinned_pkgs
    |> List.map (fun pkg -> OpamPackage.name (OpamPackage.of_string pkg))
    |> OpamPackage.Name.Set.of_list
  in
  Log.info log "Solving for %a" Fmt.(list ~sep:comma string) root_pkgs;
  let serious_errors = ref [] in
  let*! root_pkgs = parse_opams request.root_pkgs in
  let*! pinned_pkgs = parse_opams request.pinned_pkgs in
  let*! packages = Stores.packages t.stores opam_repository_commits in
  let results =
    platforms
    |> Fiber.List.map (fun (id, vars) ->
        let result =
          solve_for_platform t id
            ?cancelled
            ~cacheable
            ~log
            ~opam_repository_commits
            ~packages
            ~root_pkgs
            ~pinned_pkgs
            ~pins
            ~vars
        in
        (id, result)
      )
    |> List.filter_map (fun (id, result) ->
        Log.info log "= %s =" id;
        match result with
        | Ok result ->
          Log.info log "-> @[<hov>%a@]"
            Fmt.(list ~sep:sp string)
            result.Selection.packages;
          Log.info log "(valid since opam-repository commit(s): @[%a@])"
            Fmt.(list ~sep:semi (pair ~sep:comma string string))
            result.Selection.commits;
          Some result
        | Error (`No_solution msg) ->
          Log.info log "%s" msg;
          None
        | Error (`Msg msg) ->
          Log.info log "%s" msg;
          serious_errors := msg :: !serious_errors;
          None
      )
  in
  match cancelled with
  | Some p when Promise.is_resolved p -> Error `Cancelled
  | _ ->
    match !serious_errors with
    | [] -> Ok results
    | errors -> Fmt.error_msg "@[<v>%a@]" Fmt.(list ~sep:cut string) errors

let solve ?cacheable ?cancelled t ~log request =
  match cacheable with
  | Some true ->
    let solve = solve t ?cancelled ~cacheable:true in
    let cache = Solve_cache.create ~cache_dir:t.cache_dir ~proc_mgr:t.process_mgr in 
    Solve_cache.solve cache ~solve log request
  | _ -> solve ?cancelled ~cacheable:false t ~log request

let create ~sw ~domain_mgr ~process_mgr ~cache_dir ~n_workers =
  let stores = Stores.create ~process_mgr ~cache_dir in
  let pool = Pool.create ~sw ~domain_mgr ~n_workers Domain_worker.solve in
  {
    stores;
    pool;
    cache_dir;
    process_mgr = (process_mgr :> [`Generic] Eio.Process.mgr_ty r);
  }
