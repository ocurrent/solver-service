open Eio.Std

module Worker = Solver_service_api.Worker
module Log_data = Solver_service_api.Solver.Log
module Cache = Scache.Cache
module Set = Set.Make(Worker.Selection)

type t = {
  cache_dir : string;
  process_mgr : [`Generic] Eio.Process.mgr_ty r;
}

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

(* module Log = struct *)
(*   (* let src = Logs.Src.create "solver-worker" ~doc:"solver worker agent" *) *)
(*   let src = Logs.Src.create "solver-scache" ~doc:"solver cache system" *)
(*   include (val Logs.src_log src : Logs.LOG) *)
(* end *)

let mutex = Lazy.from_fun (fun () -> Eio.Mutex.create ())

let git_command ?cwd args =
  "git" ::
  match cwd with
  | Some dir -> "-C" :: dir :: args
  | None -> args

let find_command ?cwd args =
  "find" ::
  match cwd with
  | Some dir -> dir :: args
  | None -> args

let grep_command ?cwd args = let _ = cwd in "grep" :: args

let run_git ?cwd t args =
  Eio.Process.run t.process_mgr (git_command ?cwd args)

let take_all_opt r =
  if Eio.Buf_read.at_end_of_input r then None
  else Some (Eio.Buf_read.take_all r)

let run_take_all ?cwd ?stdin t args command =
  Eio.Process.parse_out ?stdin t.process_mgr take_all_opt (command ?cwd args)

let lines_opt r =
  if Eio.Buf_read.at_end_of_input r then None
  else (Eio.Buf_read.map List.of_seq Eio.Buf_read.lines) r |> Option.some

let run_lines ?cwd ?stdin t args command =
  Eio.Process.parse_out ?stdin t.process_mgr lines_opt (command ?cwd args)

let run_git_lines ?cwd ?stdin t args = run_lines ?cwd ?stdin t args git_command

let run_git_take_all ?cwd ?stdin t args = run_take_all ?cwd ?stdin t args git_command 

let run_grep_lines ?cwd ?stdin t args = run_lines ?cwd ?stdin t args grep_command

let run_find_take_all ?cwd ?stdin t args = run_take_all ?cwd ?stdin t args find_command 

module Git_clone = struct

  let replace_special =
    String.map @@ function
    | 'A'..'Z'
    | 'a'..'z'
    | '0'..'9'
    | '-' as c -> c
    | _ -> '_'
let rec mkdir_p path =
    try Unix.mkdir path 0o700 with
    | Unix.Unix_error (EEXIST, _, _) -> ()
    | Unix.Unix_error (ENOENT, _, _) ->
      let parent = Filename.dirname path in
      mkdir_p parent;
      Unix.mkdir path 0o700

  let repo_url_to_clone_path t repo_url =
    let solve_dir = "solve" in
    let uri = Uri.of_string repo_url in
    let sane_host =
      match Uri.host uri with
      | Some host -> replace_special host
      | None -> "no_host"
    in
    let sane_path =
      Uri.(
        path uri
        |> pct_decode
        |> Filename.chop_extension
        |> replace_special)
    in
    Fpath.(v t.cache_dir / solve_dir / sane_host / sane_path)

  let clone t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url in
    let clone_parent = Fpath.parent clone_path |> Fpath.to_string in
    let clone_path_str = Fpath.to_string clone_path in
    match Unix.lstat clone_path_str with
    | Unix.{ st_kind = S_DIR; _ } -> ()
    | _ -> Fmt.failwith "%S is not a directory!" clone_path_str
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p clone_parent;
      try
        run_git t ["clone"; repo_url; clone_path_str; "--branch"; "master"]
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "cloning %S" repo_url

  let pull t repo_url =
    try
      let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
      run_git t ~cwd:clone_path ["pull"; "origin"]
    with Eio.Io _ as ex ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt "pulling %S" repo_url

  let all_commits_rev t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_lines t ~cwd:clone_path
    @@ "log"
       :: "--reverse"
       :: [ "--format=format:%H" ]
    |> Option.value ~default:[]

  let _log t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_take_all t ~cwd:clone_path
    @@ ["log"]
    |> Option.value ~default:"Nothing"

  let _branch t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_take_all t ~cwd:clone_path
    @@ ["branch"]
    |> Option.value ~default:"Nothing"

  let _reflog t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_take_all t ~cwd:clone_path
    @@ ["reflog"]
    |> Option.value ~default:"Nothing"

  let find t repo_url =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_find_take_all t ~cwd:clone_path
    @@ []
    |> Option.value ~default:"Nothing"

  let diff t ~repo_url ~new_commit ~old_commit =
    let clone_path = repo_url_to_clone_path t repo_url |> Fpath.to_string in
    run_git_take_all t ~cwd:clone_path
    @@ "diff"
       :: old_commit
       :: new_commit
       :: "--"
       :: [ "packages" ]
    |> function
    | None -> []
    | Some diff ->
      try
        run_grep_lines ~stdin:(Eio.Flow.string_source diff) t ["^... ./packages/.*/opam"] |> Option.value ~default:[]
        |> List.map (fun path ->
          Astring.String.cuts ~rev:true ~sep:"/" path 
          |> function
          | _::_::package::_ ->  package
          | _ -> Fmt.failwith "Pkgs diff between %s and %s of %s@." repo_url new_commit old_commit)
      with _ -> [] (* grep could exits with status 1 *)
end

let cache = Cache.start ~name:"solve"

let digest_request request =
  request
  |> Worker.Solve_request.to_yojson 
  |> Yojson.Safe.to_string
  |> Digest.string
  |> Digest.to_hex

let _digest_solve_response ~solve_response =
  solve_response
  |> Worker.Solve_response.to_yojson
  |> Yojson.Safe.to_string
  |> Digest.string

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

let sort_by_url opam_repository_commits = 
  opam_repository_commits
  |> List.sort (fun (url1,_) (url2,_) -> String.compare url1 url2)

let _digest_opam_commits opam_repository_commits =
  opam_repository_commits
  |> List.fold_left (fun acc (_,commit) -> acc^commit) ""
  |> Digest.string

let is_same_solution ~solve_response_cache ~solve_response =
  match solve_response_cache, solve_response with
  | Error _, _ -> false
  | _, Error _ -> false
  | Ok selections_cache, Ok selections ->
    Set.equal (Set.of_list selections_cache) (Set.of_list selections)

let yojson_of_list l = l |> [%to_yojson: string list]
let yojosn_to_list l = l |> [%of_yojson: string list]

  
(* opam-repository comit with their rank *)
let opam_commits = Lazy.from_fun (fun () -> Hashtbl.create 10)

let update_commits t repo_url commit =
  let opam_commits = Lazy.force opam_commits in
  let mutex = Lazy.force mutex in
  match Hashtbl.find_opt opam_commits commit with
  | Some _ -> ()
  | None -> (
    Eio.Mutex.use_rw mutex ~protect:true (fun () ->
      Git_clone.clone t repo_url;
      Git_clone.pull t repo_url; 
      Git_clone.all_commits_rev t repo_url)
    |> List.iteri (fun rank commit -> Hashtbl.replace opam_commits commit rank))

let changed_packages t ~new_opam_repo ~old_opam_repo =
  let opam_commits = Lazy.force opam_commits in
  try
    List.combine new_opam_repo old_opam_repo
    |> List.map (fun ((repo_url,new_commit), (_,old_commit)) ->
      let key = ("diff"^new_commit^"-"^old_commit) in
      match Cache.get cache ~key with
      | Some pkgs -> Yojson.Safe.from_string pkgs |> yojosn_to_list |> Result.get_ok
      | None -> (
        update_commits t repo_url new_commit;
        update_commits t repo_url old_commit;
        (* Fmt.pr "DIFF new=%s old=%s :%a@." new_commit old_commit *)
        (*   Fmt.(list string) (Git_clone.diff t ~repo_url ~new_commit ~old_commit); *)
        (* Fmt.pr "ALL COMMITS = %a@." Fmt.(list string) (Git_clone.all_commits t repo_url); *)
        (* Fmt.pr "FIND = %s@." (Git_clone.find t repo_url); *)
        (* Fmt.pr "BRANCH = %s@." (Git_clone.branch t repo_url); *)
        (* Fmt.pr "REFLOG = %s@." (Git_clone.reflog t repo_url); *)
        (* Fmt.pr "LOG %s@." (Git_clone.log t repo_url); *)
        match Hashtbl.find_opt opam_commits new_commit, Hashtbl.find_opt opam_commits old_commit with
        | Some rank_new, Some rank_old ->
          (* With the rank, we make sure the new_commit is newer in the opam-repo git history *)
          if rank_new > rank_old then
            let pkgs = Git_clone.diff t ~repo_url ~new_commit ~old_commit in
            Cache.set cache ~key ~value:(Yojson.Safe.to_string (yojson_of_list pkgs));
            pkgs
          else []
        | None, _ ->
            Fmt.failwith "The repo %s has not the commit %s@." repo_url new_commit
        | _, None ->
            Fmt.failwith "The repo %s has not the commit %s@." repo_url old_commit))
    |> List.flatten
    |> Option.some
  with Failure er ->
    Fmt.epr "%s" er; None

let get_names = OpamFormula.fold_left (fun a (name, _) -> name :: a) []

let deps opam_pkgs =
  opam_pkgs
  |> List.map (fun (_, content) ->
    OpamFile.OPAM.read_from_string content |> OpamFile.OPAM.depends |> get_names)

let is_invalidated t ~request ~solve_cache =
  let {
    Worker.Solve_request.opam_repository_commits;
    root_pkgs;
    pinned_pkgs; _ } = request 
  in
  let request_pkgs =
    List.concat_map (fun pkgs -> deps pkgs) [root_pkgs; pinned_pkgs]
    |> List.flatten
    |> OpamPackage.Name.Set.of_list
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
      OpamPackage.Name.Set.mem (OpamPackage.Name.of_string pkg) request_pkgs)
    |> Option.is_some

(** TODO describe solve funciton *)
let solve t solve log (request: Worker.Solve_request.t) =
  let request =
    { request with opam_repository_commits = sort_by_url request.opam_repository_commits }
  in
  let solve () = solve ~log request in
  match get_solve ~cache ~digest:(digest_request request) with
  | Some solve_cache when Result.is_ok solve_cache.solve_response -> (
    (* Log.info (fun f -> f "Solve from cache with exact opam-commits"); *)
    Log_data.info log "From cache@.";
    solve_cache.solve_response)
  | _ -> (
    let req =
      { request with opam_repository_commits = remove_commits request.opam_repository_commits }
    in
    match get_solve ~cache ~digest:(digest_request req) with
    | Some solve_cache when not (Result.is_error solve_cache.solve_response || is_invalidated t ~request ~solve_cache) ->
      (* Log.info (fun f -> f "Solve from cache (the old solve wasn't invalidated"); *)
      Log_data.info log "From cache@.";
      let solve_cache =
        { solve_cache with last_opam_repository_commits = request.opam_repository_commits }
      in
      set_solve ~cache ~solve_cache ~digest:(digest_request req);
      set_solve ~cache ~solve_cache ~digest:(digest_request request);
      solve_cache.solve_response
    | Some solve_cache when Result.is_ok solve_cache.solve_response ->
      (* Log.info (fun f -> f "Invalidated solve from cache"); *)
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
      (* Log.info (fun f -> f "Solve not from cache"); *)
      let solve_response = solve () in
      let solve_cache =
        { Solve_cache.request; solve_response; last_opam_repository_commits = request.opam_repository_commits }
      in
      set_solve ~cache ~digest:(digest_request req) ~solve_cache;
      set_solve ~cache ~digest:(digest_request request) ~solve_cache;
      solve_response
  )
