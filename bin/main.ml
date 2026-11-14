open CamelSnake

let set_server handler request = 
  let%lwt response = handler request in
  Dream.add_header response "server" "jprili/camelsnake";
  Lwt.return response
;;

let () = 
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" on_info;
    Dream.post "/start" on_start;
    Dream.post "/move"  on_move;
    Dream.post "/end"   on_end;
  ]
