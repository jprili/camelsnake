open Types
open CoordSet
open Collision

type cell =
  | Unclaimed
  | Blocked
  | Owned of { snake_id : string; dist : int; length : int }

let flood_fill_prune width height hazards safe_moves =
  let flood_fill coord =
    let visited = Array.make_matrix width height false in
    let rec count x y =
      if (not (is_safe width height hazards { x; y })) || visited.(x).(y) then
        0
      else (
        visited.(x).(y) <- true;
        1 + count (x - 1) y + count (x + 1) y + count x (y - 1) + count x (y + 1))
    in
    count coord.x coord.y
  in
  let areas =
    List.map
      (fun (_, coord) ->
        Dream.log "%i %i" coord.x coord.y;
        flood_fill coord)
      safe_moves
  in
  Dream.log "%s" (List.map string_of_int areas |> String.concat "; ");
  let max_area = List.fold_left max 0 areas in
  List.filter_map
    (fun ((dir, _), area) ->
      if area = max_area then
        Some dir
      else
        None)
    (List.combine safe_moves areas)


let define_territories (board : board) =
  let width = board.width in
  let height = board.height in
  let snakes = board.snakes in
  let grid = Array.make_matrix width height Unclaimed in
  let queue = Queue.create () in

  let in_bounds x y =
    x >= 0 && x < width && y >= 0 && y < height
  in


  List.iter
    (fun (snake : snake) ->
      List.iter
        (fun (coord : coord) ->
          if in_bounds coord.x coord.y then
            grid.(coord.x).(coord.y) <- Blocked)
        snake.body)
    snakes;


  let better_tie candidate_id candidate_length you_id you_length =
    candidate_length > you_length
    || (candidate_length = you_length
       && String.compare candidate_id you_id < 0)
  in


  let seed (snake : snake) =
    let x = snake.head.x in
    let y = snake.head.y in
    if in_bounds x y then (
      let source =
        Owned
          {
            snake_id = snake.id; dist = 0; length = snake.length;
          }
      in
      match grid.(x).(y) with
      | Blocked | Unclaimed ->
          grid.(x).(y) <- source;
          Queue.add (x, y, snake.id, 0, snake.length) queue
      | Owned current
        when current.dist > 0 || better_tie snake.id snake.length current.snake_id current.length ->
          grid.(x).(y) <- source;
          Queue.add (x, y, snake.id, 0, snake.length) queue
      | Owned _ -> ())
  in

  List.iter seed snakes;

  let dirs = [ (0, 1); (0, -1); (-1, 0); (1, 0) ] in

  while not (Queue.is_empty queue) do
    let x, y, snake_id, dist, length = Queue.take queue in


    let is_current =
      match grid.(x).(y) with
      | Owned current ->
          current.snake_id = snake_id && current.dist = dist && current.length = length
      | Unclaimed | Blocked -> false
    in

    if is_current then
      List.iter
        (fun (dx, dy) ->
          let nx = x + dx in
          let ny = y + dy in
          if in_bounds nx ny then (
            let next_dist = dist + 1 in
            let should_claim =
              match grid.(nx).(ny) with
              | Unclaimed -> true
              | Blocked -> false
              | Owned current ->
                  next_dist < current.dist || (next_dist = current.dist && better_tie snake_id length current.snake_id current.length)
            in

            if should_claim then (
              grid.(nx).(ny) <-
                Owned { snake_id; dist = next_dist; length };
              Queue.add (nx, ny, snake_id, next_dist, length) queue
            )
          ))
        dirs
  done;
     grid
