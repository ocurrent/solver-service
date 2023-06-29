open Eio.Std

type ('request, 'reply) t = ('request * 'reply Promise.u) Eio.Stream.t

let rec run_worker t handle =
  let request, set_reply = Eio.Stream.take t in
  handle request |> Promise.resolve set_reply;
  run_worker t handle

let create ~sw ~domain_mgr ~n_workers handle =
  let t = Eio.Stream.create 0 in
  for _i = 1 to n_workers do
    Fiber.fork_daemon ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_worker t handle)
      )
  done;
  t

let use t request =
  let reply, set_reply = Promise.create () in
  Eio.Stream.add t (request, set_reply);
  Promise.await reply
