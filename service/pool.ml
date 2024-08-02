open Eio.Std

type ('request, 'reply) t = {
  work : ('request * 'reply Promise.u) Eio.Stream.t;
  do_main : Eio.Condition.t;
}

let rec run_worker t handle =
  let request, set_reply = Eio.Stream.take t.work in
  handle request |> Promise.resolve set_reply;
  run_worker t handle

(* OCaml currently requires all domains to synchronise for minor GCs.
   Idle domains must be woken by the OS, which is slow.
   As a work-around, we also run jobs in the main domain so it doesn't become idle.
   This makes the service less responsive (e.g. at reporting progress messages)
   but increases throughput. *)
let run_main_worker t handle =
  while true do
    for _ = 1 to 10 do Fiber.yield () done;
    match Eio.Stream.take_nonblocking t.work with
    | None -> Eio.Condition.await_no_mutex t.do_main
    | Some (request, set_reply) -> handle request |> Promise.resolve set_reply
  done

let create ~sw ~domain_mgr ~n_workers ~main_does_work handle =
  let t = {
    work = Eio.Stream.create 0;
    do_main = Eio.Condition.create ();
  } in
  if main_does_work then
    Fiber.fork_daemon ~sw (fun () -> run_main_worker t handle);
  for _i = 1 to n_workers do
    Fiber.fork_daemon ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_worker t handle)
      )
  done;
  t

let use t request =
  let reply, set_reply = Promise.create () in
  Eio.Stream.add t.work (request, set_reply);
  Promise.await reply

(* Prod [run_main_worker] to run soon.
   The service will call [use] multiple times after this,
   adding to [work], which will get picked up first by the
   worker domains. [run_main_worker] will then resume after that,
   collecting any left-over work. *)
let wake_main_later t =
  Eio.Condition.broadcast t.do_main
