open Types
open CoordSet

let ( >> ) f g x = x |> f |> g
let all_directions = [ Up; Down; Left; Right ]

let without_tail body =
  (* returns the body in rev order, but it doesn't matter *)
  match List.rev body with
  | [] -> []
  | _tail :: rest -> rest

let current_hazards board =
  let snake_bodies = board.snakes |> List.concat_map (fun { body } -> body) in
  let hazards =
    match board.hazards with
    | None -> []
    | Some l -> l
  in
  CoordSet.of_list (snake_bodies @ hazards)

let move head = function
  | Up -> { x = head.x; y = head.y + 1 }
  | Down -> { x = head.x; y = head.y - 1 }
  | Left -> { x = head.x - 1; y = head.y }
  | Right -> { x = head.x + 1; y = head.y }

let is_safe width height hazards next_head =
  next_head.x >= 0
  && next_head.x < width
  && next_head.y >= 0
  && next_head.y < height
  && not (CoordSet.mem next_head hazards)

(** Returns a list of possible body parts for the snake's next stage *)
let next_body board your_id hazards snake =
  match (snake.id, snake.body) with
  | id, body when your_id = id -> body |> without_tail |> CoordSet.of_list
  | _, [] -> failwith "snake body should not be empty"
  | _, (head :: rest as body) ->
      let next_head = move head in
      let next_body next_head =
        let { width; height } = board in
        if not (is_safe width height hazards next_head) then
          None
        else if List.mem next_head board.food then
          Some (next_head :: body)
        else
          Some (next_head :: body |> without_tail)
      in
      all_directions
      |> List.filter_map (next_head >> next_body)
      |> List.concat
      |> CoordSet.of_list

let get_hazards board your_id =
  let current_bodies =
    board.snakes
    |> List.concat_map (fun snake -> snake.body)
    |> CoordSet.of_list
  in
  let board_hazards =
    match board.hazards with
    | None -> CoordSet.empty
    | Some l -> CoordSet.of_list l
  in
  let current_hazards = CoordSet.union current_bodies board_hazards in
  let next_bodies =
    board.snakes
    |> List.map (next_body board your_id current_hazards)
    |> CoordSet.union_list
  in
  CoordSet.union next_bodies board_hazards

let get_safe_moves board hazards head =
  all_directions
  |> List.filter_map (fun dir ->
      let next_head = move head dir in
      let { width; height } = board in
      match is_safe width height hazards next_head with
      | true -> Some (dir, next_head)
      | false -> None)
