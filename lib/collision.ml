open Types
open CoordSet

let ( >> ) f g x = x |> f |> g

let legal_directions body =
  let curr_head, prev_head =
    match body with
    | a :: b :: _ -> (a, b)
    | a :: [] -> (a, a)
    | _ -> failwith "snake body should not be empty"
  in

  if curr_head.y < prev_head.y then
    [ Down; Left; Right ]
  else if curr_head.y > prev_head.y then
    [ Up; Left; Right ]
  else if curr_head.x > prev_head.x then
    [ Up; Down; Right ]
  else if curr_head.x < prev_head.x then
    [ Up; Down; Left ]
  else
    [ Up; Down; Left; Right ]

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

(** Returns a list of possible body parts for the snake's next stage *)
let next_body { width; height; food } your_id hazards snake =
  match (snake.id, snake.body) with
  | id, body when your_id = id -> body |> without_tail |> CoordSet.of_list
  | _, [] -> failwith "snake body should not be empty"
  | _, (head :: rest as body) ->
      let next_head = function
        | Up -> { x = head.x; y = head.y + 1 }
        | Down -> { x = head.x; y = head.y - 1 }
        | Left -> { x = head.x - 1; y = head.y }
        | Right -> { x = head.x + 1; y = head.y }
      in
      let next_body next_head =
        if
          next_head.x < 0
          || next_head.x >= width
          || next_head.y < 0
          || next_head.y >= height
          || CoordSet.mem next_head hazards
        then
          None
        else if List.mem next_head food then
          Some (next_head :: body)
        else
          Some (next_head :: body |> without_tail)
      in
      body
      |> legal_directions
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
