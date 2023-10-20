open Eio.Std

type ('request, 'reply) t = {
  requests: ('request * 'reply Promise.u) Eio.Stream.t; running: int Atomic.t; n_workers: int
}

let rec run_worker t handle =
  let request, set_reply = Eio.Stream.take t.requests in
  handle request |> Promise.resolve set_reply;
  run_worker t handle

let create ~sw ~domain_mgr ~n_workers handle =
  let t = { requests = Eio.Stream.create max_int; running = Atomic.make 0; n_workers } in
  for _i = 1 to n_workers do
    Fiber.fork_daemon ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_worker t handle)
      )
  done;
  t

let use t request =
  let reply, set_reply = Promise.create () in
  Eio.Stream.add t.requests (request, set_reply);
  Promise.await reply

let n_workers t = t.n_workers

let wait_requests t = Eio.Stream.length t.requests
