open Eio.Std

module Worker = Solver_service_api.Worker
module Log_data = Solver_service_api.Solver.Log
module Cache = Scache.Cache
module Selections = Set.Make(Worker.Selection)

exception Invalidated

type t = {
  cache_dir : string;
  process_mgr : [`Generic] Eio.Process.mgr_ty r;
}

let create ~cache_dir ~proc_mgr =
  {
    cache_dir = Fpath.(v cache_dir / "solve") |> Fpath.to_string;
    process_mgr = proc_mgr
  }

module Git_clone = Git_clone.Make ( struct
    type cache = t
    let dir t = t.cache_dir
    let process_mgr t = t.process_mgr
end)

module Solve_cache = struct

  type t = {
    request: Worker.Solve_request.t;
    (** The request *)
    solve_response : Worker.Solve_response.t;
    (** Response of the solved request *)
    last_opam_repository_commits: (string * string) list;
    (** Pair of repo URL and commit hash, for each last opam-repository used during the request *)
  }[@@deriving yojson]

  let marshal t = t |> to_yojson |> Yojson.Safe.to_string
  let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok

end

let mutex = Lazy.from_fun (fun () -> Eio.Mutex.create ())

let cache = Cache.start ~name:"solve"

let digest_request request =
  request
  |> Worker.Solve_request.to_yojson 
  |> Yojson.Safe.to_string
  |> Digest.string
  |> Digest.to_hex

let get_solve ~cache ~digest : Solve_cache.t option =
  match Cache.get cache ~key:digest with
  | None -> None
  | Some r -> Solve_cache.unmarshal r |> Option.some

let set_solve ~cache ~solve_cache ~digest =
  solve_cache
  |> Solve_cache.marshal
  |> fun response -> Cache.set cache ~key:digest ~value:response

let remove_commits opam_repository_commits =
  opam_repository_commits |> List.map (fun (url,_) -> (url,""))

(* important because of the digest_request *)
let sort_by_url opam_repository_commits = 
  opam_repository_commits
  |> List.sort (fun (url1,_) (url2,_) -> String.compare url1 url2)

let is_same_solution ~solve_response_cache ~solve_response =
  match solve_response_cache, solve_response with
  | Error _, _ -> false
  | _, Error _ -> false
  | Ok selections_cache, Ok selections ->
    let selections_cache =
      List.map (fun sel -> { sel with Worker.Selection.commits = []}) selections_cache
    in
    let selections =
      List.map (fun sel -> { sel with Worker.Selection.commits = []}) selections
    in
    Selections.equal (Selections.of_list selections_cache) (Selections.of_list selections)

let yojson_of_list l = l |> [%to_yojson: string list]
let yojosn_to_list l = l |> [%of_yojson: string list]
  
(* opam-repo comits with their rank *)
let opam_commits = Lazy.from_fun (fun () -> Hashtbl.create 10)

let update_commit t repo_url commit =
  let opam_commits = Lazy.force opam_commits in
  let mutex = Lazy.force mutex in
  let get_repo t repo_url =
    Git_clone.clone t repo_url;
    Git_clone.pull t repo_url; 
    Git_clone.all_commits_rev t repo_url
  in
  match Hashtbl.find_opt opam_commits commit with
  | Some _ -> ()
  | None -> 
    Eio.Mutex.use_rw mutex ~protect:true (fun () ->
      try
        get_repo t repo_url;
      with _ -> (
        (* could be a conflict between commits when pulling *)
        Git_clone.remove t repo_url;
        get_repo t repo_url))
    |> List.iteri (fun rank commit -> Hashtbl.replace opam_commits commit rank)

let changed_packages t ~new_opam_repo ~old_opam_repo =
  if new_opam_repo = old_opam_repo then
    Some []
  else
    let opam_commits = Lazy.force opam_commits in
    try
      (* new_opam_repo and old_opam_repo nead to be sorted by url *)
      List.combine new_opam_repo old_opam_repo
      |> List.map (fun ((repo_url,new_commit), (_,old_commit)) ->
        let key = ("diff"^new_commit^"-"^old_commit) in
        match Cache.get cache ~key with
        | Some pkgs -> Yojson.Safe.from_string pkgs |> yojosn_to_list |> Result.get_ok
        | None -> (
          update_commit t repo_url new_commit;
          update_commit t repo_url old_commit;
          match Hashtbl.find_opt opam_commits new_commit, Hashtbl.find_opt opam_commits old_commit with
          | Some new_rank, Some old_rank when new_rank < old_rank ->
            (* This new commit is supposed to be newer in the commit history,
             this could be a specific request on opam commits, like fixed demand
             so it invalidated *)
            raise Invalidated
          | Some _, Some _ ->
            let pkgs_filename = Git_clone.diff_pkgs t ~repo_url ~new_commit ~old_commit in
            Cache.set cache ~key ~value:(Yojson.Safe.to_string (yojson_of_list pkgs_filename));
            pkgs_filename
          | None, _ ->
            Fmt.epr "The repo %s has not the commit %s@." repo_url new_commit; raise Invalidated
          | _, None ->
            Fmt.epr "The repo %s has not the commit %s@." repo_url old_commit; raise Invalidated))
      |> List.flatten
      |> Option.some
    with Invalidated -> None

let get_names = OpamFormula.fold_left (fun a (name, _) -> name :: a) []

let deps_of_opam_file opam_pkgs =
  opam_pkgs
  |> List.map (fun (_, content) ->
    OpamFile.OPAM.read_from_string content |> OpamFile.OPAM.depends |> get_names)

let is_invalidated t ~request ~solve_cache =
  let {
    Worker.Solve_request.opam_repository_commits;
    root_pkgs;
    pinned_pkgs; _ } = request 
  in
  let request_pkgs () =
    List.concat_map (fun pkgs_name -> deps_of_opam_file pkgs_name) [root_pkgs; pinned_pkgs]
    |> List.flatten
    |> OpamPackage.Name.Set.of_list
  in
  let response_pkgs () =
    solve_cache.Solve_cache.solve_response
    |> Result.get_ok
    |> List.map (fun selection -> selection.Worker.Selection.packages)
    |> List.concat
    |> List.map (fun pkg_version ->
      pkg_version
      |> Astring.String.cut ~sep:"."
      |> Option.get
      |> fun (name,version) ->
        OpamPackage.create  (OpamPackage.Name.of_string name) (OpamPackage.Version.of_string version))
    |> OpamPackage.Set.of_list
  in
  let old_opam_repo =
    solve_cache.Solve_cache.last_opam_repository_commits
    |> List.sort (fun (url1,_) (url2,_) -> String.compare url1 url2)
  in
  let new_opam_repo =
    opam_repository_commits
    |> List.sort (fun (url1,_) (url2,_) -> String.compare url1 url2)
  in
  match changed_packages t ~old_opam_repo ~new_opam_repo with
  | None -> true (* Invalidate when a commit does not exist *)
  | Some pkgs ->
    pkgs
    |> List.find_opt (fun pkg ->
      let request_pkgs = request_pkgs () in
      let response_pkgs = response_pkgs () in
      OpamFilename.raw pkg
      |> OpamPackage.of_filename
      |> Option.get
      |> fun opam_pkg ->
        OpamPackage.Name.Set.mem (OpamPackage.name opam_pkg) request_pkgs || OpamPackage.Set.mem opam_pkg response_pkgs)
    |> Option.is_some

(**
    There is 2 stage of looking for the cache:
      * With opam repository URL and their commit: (url,commit) list
      * Only the opam repository URL: (url,_) list

    When the cache is hited with only opam URLs, it try to invalidate it
    because of the opam repository commit could be updated with new commit.

    The invalidation is about looking if the request packages is involve in
    the 2 different commit, the commit of the cached response and the commit of
    the request. Also the cache response contain the transitive dependencies, we
    make sure those ones are not also involve in the commit changes.

    The oldest commit used during the solve is kept when the response is the same
    as previous solve.
*)
let solve t ~solve log (request: Worker.Solve_request.t) =
  let request =
    { request with opam_repository_commits = sort_by_url request.opam_repository_commits }
  in
  let solve () = solve ~log request in
  match get_solve ~cache ~digest:(digest_request request) with
  | Some solve_cache when Result.is_ok solve_cache.solve_response -> (
    Log_data.info log "From cache@.";
    solve_cache.solve_response)
  | _ -> (
    let req =
      { request with opam_repository_commits = remove_commits request.opam_repository_commits }
    in
    match get_solve ~cache ~digest:(digest_request req) with
    | Some solve_cache when not (Result.is_error solve_cache.solve_response || is_invalidated t ~request ~solve_cache) ->
      Log_data.info log "From cache@.";
      let solve_cache =
        { solve_cache with last_opam_repository_commits = request.opam_repository_commits }
      in
      set_solve ~cache ~solve_cache ~digest:(digest_request req);
      set_solve ~cache ~solve_cache ~digest:(digest_request request);
      solve_cache.solve_response
    | Some solve_cache when Result.is_ok solve_cache.solve_response ->
      let solve_response = solve () in
      let solve_cache =
        if is_same_solution ~solve_response_cache:solve_cache.solve_response ~solve_response then
          { solve_cache with last_opam_repository_commits = request.opam_repository_commits }
        else
          {request; solve_response; last_opam_repository_commits = request.opam_repository_commits }
      in
      set_solve ~cache ~solve_cache ~digest:(digest_request req);
      set_solve ~cache ~solve_cache ~digest:(digest_request request);
      solve_cache.solve_response
    | _ ->
      let solve_response = solve () in
      let solve_cache =
        { Solve_cache.request; solve_response; last_opam_repository_commits = request.opam_repository_commits }
      in
      set_solve ~cache ~digest:(digest_request req) ~solve_cache;
      set_solve ~cache ~digest:(digest_request request) ~solve_cache;
      solve_response
  )
