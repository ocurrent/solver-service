open Eio.Std
open Lwt.Infix

module Store = Git_unix.Store
module Search = Git.Search.Make (Digestif.SHA1) (Store)

type t = OpamFile.OPAM.t OpamPackage.Version.Map.t Eio.Lazy.t OpamPackage.Name.Map.t

let empty = OpamPackage.Name.Map.empty

let with_store safe_store fn =
  Switch.run @@ fun sw ->
  let request = Safe_store.get ~sw safe_store in
  match Safe_store.get_store request with
  | Error ex -> raise ex
  | Ok store -> fn store

let read_dir store hash =
  Store.read store hash >|= function
  | Error e -> Fmt.failwith "Failed to read tree: %a" Store.pp_error e
  | Ok (Git.Value.Tree tree) -> Some tree
  | Ok _ -> None

let read_package store pkg hash =
  Search.find store hash (`Path [ "opam" ]) >>= function
  | None ->
    Fmt.failwith "opam file not found for %s" (OpamPackage.to_string pkg)
  | Some hash -> (
      Store.read store hash >|= function
      | Ok (Git.Value.Blob blob) ->
        begin
          try OpamFile.OPAM.read_from_string (Store.Value.Blob.to_string blob)
          with ex ->
            Fmt.failwith "Error parsing %s: %s" (OpamPackage.to_string pkg) (Printexc.to_string ex)
        end
      | _ ->
        Fmt.failwith "Bad Git object type for %s!" (OpamPackage.to_string pkg)
    )

(* Get a map of the versions inside [entry] (an entry under "packages") *)
let read_versions store (entry : Store.Value.Tree.entry) =
  Lwt_eio.run_eio @@ fun () ->
  with_store store @@ fun store ->
  Lwt_eio.run_lwt @@ fun () ->
  read_dir store entry.node >>= function
  | None -> Lwt.return OpamPackage.Version.Map.empty
  | Some tree ->
    Store.Value.Tree.to_list tree
    |> Lwt_list.fold_left_s
      (fun acc (entry : Store.Value.Tree.entry) ->
         match OpamPackage.of_string_opt entry.name with
         | Some pkg ->
           read_package store pkg entry.node >|= fun opam ->
           OpamPackage.Version.Map.add pkg.version opam acc
         | None ->
           OpamConsole.log "opam-0install" "Invalid package name %S"
             entry.name;
           Lwt.return acc)
      OpamPackage.Version.Map.empty

let read_packages ~store tree =
  Store.Value.Tree.to_list tree
  |> List.filter_map (fun (entry : Store.Value.Tree.entry) ->
      match OpamPackage.Name.of_string entry.name with
      | exception ex ->
        OpamConsole.log "opam-0install"
          "Invalid package name %S: %s" entry.name
          (Printexc.to_string ex);
        None
      | name ->
        Some (name, Eio.Lazy.from_fun ~cancel:`Restart (fun () -> Lwt_eio.run_lwt_in_main (fun () -> read_versions store entry)))
    )
  |> OpamPackage.Name.Map.of_list

let overlay v1 v2 =
  (* Overwrite the v1 entry. This gives the semantics that that second
     repo given to read_packages is an overlay on the first one. *)
  Eio.Lazy.from_fun ~cancel:`Restart (fun () ->
      let v1 = Eio.Lazy.force v1 in
      let v2 = Eio.Lazy.force v2 in
      OpamPackage.Version.Map.union (fun _ v2 -> v2) v1 v2
    )

let of_commit ?(super=empty) store commit : t =
  with_store store @@ fun current_store ->
  Lwt_eio.run_lwt @@ fun () ->
  Search.find current_store commit (`Commit (`Path [ "packages" ])) >>= function
  | None -> Fmt.failwith "Failed to find packages directory!"
  | Some tree_hash -> (
      read_dir current_store tree_hash >>= function
      | None -> Fmt.failwith "'packages' is not a directory!"
      | Some tree ->
        let packages = read_packages ~store tree in
        Lwt.return (OpamPackage.Name.Map.union overlay super packages)
    )

let get_versions (t:t) name =
  match OpamPackage.Name.Map.find_opt name t with
  | None -> OpamPackage.Version.Map.empty
  | Some versions -> Eio.Lazy.force versions
