open CamelSnake

let set_server handler request =
  let%lwt response = handler request in
  Dream.add_header response "server" "jprili/camelsnake";
  Lwt.return response

let () =
  let port =
    match Sys.getenv_opt "PORT" with
    | Some port -> int_of_string port
    | None -> 8080
  in
  Dream.run ~port
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" on_info;
         Dream.post "/start" on_start;
         Dream.post "/move" on_move;
         Dream.post "/end" on_end;
       ]
