open Types
open Collision

let get_diff = function
  | Up -> (0, 1)
  | Down -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let move_snake snake dir : snake =
  let no_tail = snake.body |> without_tail |> List.rev in
  let new_head = move snake.head dir in
  { snake with head = new_head; body = new_head :: no_tail }

let simulate_next snakes moves =
  List.map
    (fun (snake, dir) -> move_snake snake dir)
    (List.combine snakes moves)

let rec get_combos num dirs current =
  let out = List.map (fun x -> x :: current) dirs in
  if num <= 1 then
    out
  else
    List.concat_map (get_combos (num - 1) dirs) out

let get_next_state board = None
