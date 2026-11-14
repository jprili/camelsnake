open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Yojson.Basic.Util
open Camelsnake.Types

let list_of_moves (m : move_t) =
  [ ("up", m.up); ("down", m.down); ("left", m.left); ("right", m.right) ]

(** - All snakes are hazards
    - Map bounds are hazards
    - Hazards are hazards *)
let get_hazards game_state =
  let rec get_snake_coords (snakes : snake_t list) acc =
    match snakes with
    | snake :: rest -> get_snake_coords rest (acc @ snake.body)
    | [] -> acc
  in
  let res = get_snake_coords game_state.board.snakes [] in
  Dream.log "%s\n"
    (res
    |> List.map (fun x -> Yojson.Safe.to_string ([%yojson_of: coord_t] x))
    |> List.fold_left ( ^ ) "");

  res

(** - All snakes are hazards
    - Map bounds are hazards
    - Hazards are hazards *)
let is_hazard { x; y } w h hazards direction =
  match direction with
  | Up when y = h - 1 || List.mem { x; y = y + 1 } hazards -> true
  | Down when y = 0 || List.mem { x; y = y - 1 } hazards -> true
  | Left when x = 0 || List.mem { x = x - 1; y } hazards -> true
  | Right when x = w - 1 || List.mem { x = x + 1; y } hazards -> true
  | _ -> false

(* region CALLBACK_HANDLERS *)
let handle_move game_state =
  (* up, down, left, right*)
  let available_moves = { up = true; down = true; left = true; right = true } in
  let curr_head = game_state.you.head in
  let hazards = get_hazards game_state in
  let w = game_state.board.width in
  let h = game_state.board.height in
  let is_hazard_dir = is_hazard curr_head w h hazards in
  (* This means (0, 0) is bottom-left *)
  if is_hazard_dir Up then
    available_moves.up <- false;
  if is_hazard_dir Down then
    available_moves.down <- false;
  if is_hazard_dir Left then
    available_moves.left <- false;
  if is_hazard_dir Right then
    available_moves.right <- false;

  Dream.log "up = %s " (Bool.to_string available_moves.up);
  Dream.log "down = %s " (Bool.to_string available_moves.down);
  Dream.log "left = %s " (Bool.to_string available_moves.left);
  Dream.log "right = %s " (Bool.to_string available_moves.right);

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
    (* if no moves left default to up *)
    if Array.length safe_as_array = 0 then
      "up"
    else
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
  let game_state = body |> Yojson.Safe.from_string |> game_state_t_of_yojson in
  let result = handle_move game_state in
  Dream.json result
(* endregion *)
