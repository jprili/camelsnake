open Types
open Collision
open Territory

type state_tree = {
  board : board;
  score : int;
  children : state_tree list;
}

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

let get_next_states board =
  let { snakes } = board in
  let num_snakes = List.length snakes in
  let combos = get_combos num_snakes [ Up; Down; Left; Right ] [] in
  List.map
    (fun combo -> { board with snakes = simulate_next snakes combo })
    combos

let new_state_tree board = { board; score = 0; children = [] }

let eval board you =
  let score = ref 0 in
  let territory = define_territories board in
  for i = 0 to Array.length territory do
    for j = 0 to Array.length territory.(i) do
      match territory.(i).(j) with
      | Owned { snake_id } when snake_id = you.id -> score := !score + 1
      | _ -> ()
    done
  done;
  !score

let rec build_state_tree you_id depth { board; children } =
  let score = eval board you_id in
  if depth <= 0 then
    { board; score; children = [] }
  else
    let next_states = get_next_states board in
    {
      board;
      score;
      children =
        List.map
          (fun b -> build_state_tree you_id (depth - 1) (new_state_tree b))
          next_states;
    }
