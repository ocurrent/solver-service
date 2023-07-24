open Eio.Std

module S = Git_unix.Store

(* Normally, there is only one active generation, so everyone uses the same store.
   During an upgrade there are two generations:

   - The old one, where [users] counts how many users are using the old store.
   - The new one, where [users] counts how many users are waiting for the new store.

   The update sequence is:

   1. Create new generation (with an unresolved store).
      New users wait here for the store to become resolved.
      Mark the old generation as being finished.

   2. Old users finish using the old generation
      (the updater is also a user and releases it too).
      The last user frees the old store and notifies the updater.

   3. Now git-fetch runs. No users are using any store (all the old users have
      finished, and new ones just wait on the new generation).

   4. When the fetch completes, the promise resolves to the new store.

   It would probably be better to do the git-fetch using git-unix, which should
   avoid the need to replace the stores like this. However, it doesn't support GC. *)

type generation = {
  store : S.t Promise.or_exn;
  mutable users : int;                  (* Can be incremented only when we are the current generation. *)
  mutable notify_finished : unit Promise.u option;      (* [Some _] when we're moving to a new generation. *)
}

type t = generation ref

let make_generation store =
  { store; users = 0; notify_finished = None }

exception Not_initialised

let create () =
  ref @@ make_generation (Promise.create_resolved (Error Not_initialised))

type request = {
  t : t;
  g : generation;
  hook : Switch.hook;
  active : bool ref;
}

let release g =
  assert (g.users > 0);
  g.users <- g.users - 1;
  match g.notify_finished with
  | Some notify when g.users = 0 ->
    (* We want to shut down this generation and we were the last user. *)
    Eio.Cancel.protect (fun () ->
        Promise.peek g.store
        |> Option.get   (* Must be resolved, since someone decided they needed a newer version. *)
        |> Result.iter (fun store ->
            Lwt_eio.run_lwt (fun () -> Git_unix.Store.close_pack_files store)
          );
        Promise.resolve notify ()
      )
  | _ -> ()

let get ~sw t =
  let g = !t in
  g.users <- g.users + 1;
  let active = ref true in
  let hook = Switch.on_release_cancellable sw (fun () -> active := false; release g) in
  { t; g; hook; active }

let release req =
  assert !(req.active);
  req.active := false;
  Switch.remove_hook req.hook;
  release req.g

let get_store req =
  let store = Promise.await req.g.store in
  assert !(req.active);
  store

let upgrade req fn =
  let t = req.t in
  if !t != req.g then (
    release req;
    Error `Concurrent_upgrade
  ) else (
    let finished, set_finished = Promise.create () in
    let new_store, set_new_store = Promise.create () in
    req.g.notify_finished <- Some set_finished;
    let g2 = make_generation new_store in
    t := g2;            (* From this point, [req.g.users] cannot increase *)
    match
      release req;
      Promise.await finished;
      fn ()
    with
    | exception ex ->
      Promise.resolve_error set_new_store ex;
      Error (`Exn ex)
    | store ->
      Promise.resolve_ok set_new_store store;
      Ok ()
  )
