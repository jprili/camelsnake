open CamelSnake

let () = 
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" info;
    Dream.post "/start" on_start;
    (* Dream.post "/move"  on_move;
    Dream.post "/end"   on_end; *)
  ]
