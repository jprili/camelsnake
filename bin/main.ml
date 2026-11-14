let () = 
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/"
      (fun _ -> Dream.json "{\"hello\": \"world\"}")
  ]
