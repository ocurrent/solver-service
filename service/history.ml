open Eio.Std

module Store = Git_unix.Store
module Dir = Map.Make(String)

module Cache = struct
  type item =
    | Tree of Store.Hash.t Dir.t            (* Or [Dir.empty] if a blob rather than a tree *)
    | Commit of Store.Hash.t Git.Commit.t

  type t = (Store.Hash.t, item Promise.or_exn) Hashtbl.t

  (* Git trees are sorted arrays, which is just what we need.
     However, ocaml-git returns them as a list so we need to index them again. *)
  let map_of_entry_list =
    List.fold_left (fun acc (entry : _ Git.Tree.entry) -> Dir.add entry.name entry.node acc) Dir.empty

  let read_exn store hash = Lwt_eio.run_lwt (fun () -> Store.read_exn store hash)

  let get (t : t) store hash =
    let load () =
      let p, r = Promise.create () in
      Hashtbl.replace t hash p;
      let v =
        match read_exn store hash with
        | Commit c -> Ok (Commit c)
        | Tree t -> Ok (Tree (Git.Tree.to_list t |> map_of_entry_list))
        | _ -> Error (Failure (Fmt.str "%a is not a tree or commit hash!" Store.Hash.pp hash))
        | exception ex -> Error ex
      in
      Promise.resolve r v;
      match v with
      | Ok x -> x
      | Error ex -> raise ex
    in
    match Hashtbl.find_opt t hash with
    | None -> load ()
    | Some x ->
      match Promise.await x with
      | Ok x -> x
      | Error _ -> load ()

  let get_commit t store hash =
    match get t store hash with
    | Commit c -> c
    | _ -> Fmt.failwith "%a is not a commit hash!" Store.Hash.pp hash

  let get_tree t store hash =
    match get t store hash with
    | Tree x -> x
    | _ -> Fmt.failwith "%a is not a tree or blob hash!" Store.Hash.pp hash

  let create () = Hashtbl.create 1000
end

type t = {
  cache : Cache.t;
  store : Store.t;
  mutable from : Store.Hash.t Git.Commit.t;    (* Current oldest matching commit known *)
}

type tree = {
  hash : Store.Hash.t;        (* The hash of the tree itself *)
  dir : Store.Hash.t Dir.t;   (* A map from child entry names to their hashes (if empty, [hash] is not a tree) *)
}

(* A [path_set] is a set of paths to content that we care about.
   They are stored as a tree so we only visit each required directory once. *)
type path_set = {
  mutable children : entry Dir.t;
  mutable tree : tree;
  (* A Git tree which is known to satisfy all children
     (this often avoids needing to examine the children individually). *)
}

and entry =
  | Require of Store.Hash.t option (* Require this item to have the given hash (or not to exist, if the hash is [None]. *)
  | Step of path_set (* A directory we don't care about directly, but which is used to access things we do care about. *)

let get_tree t hash : tree =
  let dir = Cache.get_tree t.cache t.store hash in
  { hash; dir }

(* [path_set ["a/b"; "a/c"]] is the tree:
   a
   +b
   +c

   We don't currently handle the empty path "" or double slashes, as these aren't needed currently. *)
let node_of t paths =
  (* [add_segs base path] adds [path] to the set [base]. *)
  let rec add_segs base = function
    | [] -> assert false
    | x :: xs ->
      assert (x <> "");
      let prev = Dir.find_opt x base.children in
      if xs = [] then (
        (* [x] is a required path *)
        match prev with
        | Some (Require _) -> ()    (* Duplicate; ignore *)
        | Some (Step _)             (* Replace, as requiring a full match is stronger than matching some children *)
        | None ->
          let required_hash = Dir.find_opt x base.tree.dir in
          base.children <- Dir.add x (Require required_hash) base.children
      ) else (
        (* [x] is a step towards a required path *)
        match prev with
        | Some (Step step) -> add_segs step xs
        | Some (Require _) -> () (* One path is a parent of another; ignore longer path *)
        | None ->
          match Dir.find_opt x base.tree.dir with
          | Some hash ->
            let tree = get_tree t hash in
            let step = { tree; children = Dir.empty } in
            base.children <- Dir.add x (Step step) base.children;
            add_segs step xs
          | None ->
            (* The directory containing the required path doesn't exist.
               For simplicity, treat this as requiring the directory not to exist. *)
            base.children <- Dir.add x (Require None) base.children
      )
  in
  let root = {
    tree = get_tree t (Store.Value.Commit.tree t.from);
    children = Dir.empty;
  } in
  paths |> List.iter (fun path -> String.split_on_char '/' path |> add_segs root);
  root

let rec update t step tree_hash =
  if Store.Hash.equal step.tree.hash tree_hash then (
    true
  ) else (
    (* Something is different in this sub-tree, but it might not matter. Check. *)
    let new_tree = get_tree t tree_hash in
    (* Check important entries haven't changed *)
    let sub_tree_ok name paths =
      let child = Dir.find_opt name new_tree.dir in
      match paths with
      | Require h -> Option.equal Store.Hash.equal h child
      | Step step ->
        match child with
        | Some child -> update t step child
        | None ->
          (* An intermediate directory containing paths we care about no longer exists; stop here.
             In theory, it's possible that [step] only requires the paths not to exist and we could
             continue, but it's unlikely to be useful. *)
          false
    in
    if Dir.for_all sub_tree_ok step.children then (
      step.tree <- new_tree;
      true
    ) else (
      false
    )
  )

(* [optimise t paths] tries to set [t.from] to the oldest commit with [paths].
   It does this by following the first parent until something changes. *)
let rec optimise t paths =
  match Store.Value.Commit.parents t.from with
  | [] -> ()    (* No parents; must stop here *)
  | parent :: _ ->
    let commit = Cache.get_commit t.cache t.store parent in
    let root = Store.Value.Commit.tree commit in
    if update t paths root then (
      t.from <- commit;
      optimise t paths
    ) (* Else something changed; must stop here *)

(* Does a path-set require anything to be present?
   If not, we can just not use this store. *)
let rec any_targets step =
  step.children |> Dir.exists (fun _ -> function
      | Require (Some _) -> true
      | Require None -> false
      | Step step -> any_targets step
    )

let oldest_commit_with ~cache ~store ~from paths =
  let t = {
    from = Cache.get_commit cache store from;
    cache;
    store;
  } in
  let paths = node_of t paths in
  if any_targets paths then (
    optimise t paths;
    Some (Store.Hash.to_hex (Store.Value.Commit.digest t.from))
  ) else None
