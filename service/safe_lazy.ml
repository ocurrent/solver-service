open Eio.Std

type 'a t = {
  value : 'a Promise.or_exn;
  start : (unit -> unit) option Atomic.t;   (* [None] if already started *)
}

let make fn =
  let value, r = Promise.create () in
  let start = Atomic.make @@ Some (fun () ->
      match fn () with
      | x -> Promise.resolve_ok r x
      | exception ex -> Promise.resolve_error r ex
    )
  in
  { value; start }

let force t =
  match Promise.peek t.value with
  | Some (Ok v) -> v
  | Some (Error ex) -> raise ex
  | None ->
    let start = Atomic.exchange t.start None in
    (* If [t.start] was [None] then someone else got there first. *)
    Option.iter (fun fn -> fn ()) start;
    Promise.await_exn t.value
