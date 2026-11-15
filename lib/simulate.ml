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

let rec get_combos num_snakes dirs = None
let get_next_state board = None
