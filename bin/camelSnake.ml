open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Yojson.Basic.Util
open Camelsnake.Types

let list_of_moves (m : move_t) =
  [ ("up", m.up); ("down", m.down); ("left", m.left); ("right", m.right) ]

(** - All snakes are hazards
    - Map bounds are hazards
    - Hazards are hazards *)
let is_hazard direction { x; y } w h hazards =
  match direction with
  | Up when y + 1 = h -> false
  | Down when y = 0 -> false
  | Left when x = 0 -> false
  | Right when x = w -> false
  | _ -> true

(* region CALLBACK_HANDLERS *)
let handle_move game_state =
  (* up, down, left, right*)
  let available_moves = { up = true; down = true; left = true; right = true } in
  let curr_head, prev_head =
    let default = { x = 0; y = 0 } in
    match game_state.you.body with
    | a :: b :: _ -> (a, b)
    | a :: [] -> (a, a)
    | _ -> (default, default)
  in
  (*
  TODO: Step 1 - Prevent your Battlesnake from moving out of bounds
  let board_width = game_state.board.width in
  let board_height = game_state.board.height in
  *)

  (*
let board_width = game_state.board.width in
  let board_height = game_state.board.height in
*)

  (* This means (0, 0) is bottom-left *)
  if curr_head.y < prev_head.y then
    available_moves.up <- false
  else if curr_head.y > prev_head.y then
    available_moves.down <- false
  else if curr_head.x > prev_head.x then
    available_moves.left <- false
  else if curr_head.x < prev_head.x then
    available_moves.right <- false;

  (*
  TODO: Step 2 - Prevent your Battlesnake from colliding with itself
  let my_body = game_state.you.body in
*)

  (*
  TODO: Step 3 - Prevent your Battlesnake from colliding with other Battlesnakes
  let opponents = game_state.board.snakes in
*)
  let safe_moves =
    list_of_moves available_moves
    |> List.filter_map (function
      | dir, true -> Some dir
      | _ -> None)
  in
  let selected_move =
    let safe_as_array = Array.of_list safe_moves in
    Array.get safe_as_array (safe_as_array |> Array.length |> Random.int)
  in
  [%yojson_of: move_response_t] { move = selected_move }
  |> Yojson.Safe.to_string

(* endregion *)

(* region CALLBACKS *)
let on_info _ =
  {
    apiversion = "1";
    author = "jprili";
    color = "#c0ffee";
    head = "default";
    tail = "default";
  }
  |> yojson_of_info_response
  |> Yojson.Safe.to_string
  |> Dream.json

let on_start _ =
  (**
     let%lwt body = Dream.body request in let game_state = body |>
     Yojson.Safe.from_string |> game_state_t_of_yojson in
  *)
  Dream.log "GAME START";
  "ok" |> Dream.json

let on_end _ =
  (**
     let%lwt body = Dream.body request in let game_state = body |>
     Yojson.Safe.from_string |> game_state_t_of_yojson in
  *)
  Dream.log "GAME OVER\n";
  "ok" |> Dream.json

let on_move request =
  let%lwt body = Dream.body request in
  Dream.log "%s" body;
  let game_state = body |> Yojson.Safe.from_string |> game_state_t_of_yojson in
  let result = handle_move game_state in
  Dream.json result
(* endregion *)
