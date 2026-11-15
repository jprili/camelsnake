open Types

type cell =
  | Unclaimed
  | Blocked
  | Owned of {
      snake_id : string;
      dist : int;
      length : int;
    }

let in_bounds w h x y = x >= 0 && y >= 0 && x < w && y < h

let define_territories (board : board) =
  let { width; height; snakes } = board in
  let grid = Array.make_matrix width height Unclaimed in
  let queue = Queue.create () in
  List.iter
    (fun (snake : snake) ->
      List.iter
        (fun { x; y } ->
          if in_bounds width height x y then
            grid.(x).(y) <- Blocked)
        snake.body)
    snakes;

  let better_tie candidate_id candidate_length you_id you_length =
    candidate_length > you_length
    || (candidate_length = you_length && String.compare candidate_id you_id < 0)
  in

  let seed w h (snake : snake) =
    let { x; y } = snake.head in
    if in_bounds w h x y then
      let source =
        Owned { snake_id = snake.id; dist = 0; length = snake.length }
      in
      match grid.(x).(y) with
      | Blocked | Unclaimed ->
          grid.(x).(y) <- source;
          Queue.add (x, y, snake.id, 0, snake.length) queue
      | Owned current
        when current.dist > 0
             || better_tie snake.id snake.length current.snake_id current.length
        ->
          grid.(x).(y) <- source;
          Queue.add (x, y, snake.id, 0, snake.length) queue
      | Owned _ -> ()
  in
  List.iter (seed width height) snakes;
  let dirs = [ (0, 1); (0, -1); (-1, 0); (1, 0) ] in
  while not (Queue.is_empty queue) do
    let x, y, snake_id, dist, length = Queue.take queue in
    let is_current =
      match grid.(x).(y) with
      | Owned current ->
          current.snake_id = snake_id
          && current.dist = dist
          && current.length = length
      | Unclaimed | Blocked -> false
    in
    if is_current then
      List.iter
        (fun (dx, dy) ->
          let nx = x + dx in
          let ny = y + dy in
          if in_bounds width height nx ny then
            let next_dist = dist + 1 in
            let should_claim =
              match grid.(nx).(ny) with
              | Unclaimed -> true
              | Blocked -> false
              | Owned current ->
                  next_dist < current.dist
                  || next_dist = current.dist
                     && better_tie snake_id length current.snake_id
                          current.length
            in
            if should_claim then (
              grid.(nx).(ny) <- Owned { snake_id; dist = next_dist; length };
              Queue.add (nx, ny, snake_id, next_dist, length) queue))
        dirs
  done;
  grid
