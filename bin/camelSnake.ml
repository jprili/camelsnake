open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type info_response = {
  apiversion : string;
  author : string;
  color : string;
  head : string;
  tail : string;
}
[@@deriving yojson]

type royale_t = { shrinkEveryNTurns : int } [@@deriving yojson]
(** As specified in the API docs *)

type squad_t = {
  allowBodyCollisions : bool;
  sharedElimination : bool;
  sharedHealth : bool;
  sharedLength : bool;
}
[@@deriving yojson]

type settings_t = {
  foodSpawnChance : int;
  minimumFood : int;
  hazardDamagePerTurn : int option; [@yojson.option]
  hazardMap : string option; [@yojson.option]
  hazardMapAuthor : string option; [@yojson.option]
  royale : royale_t option; [@yojson.option]
  squad : squad_t option; [@yojson.option]
}
[@@deriving yojson]

type ruleset_t = {
  name : string;
  settings : settings_t;
  version : string option; [@yojson.option]
}
[@@deriving yojson]

type game_t = {
  id : string;
  ruleset : ruleset_t;
  map : string;
  timeout : int;
  source : string option; [@yojson.option]
}
[@@deriving yojson]

type coord_t = {
  x : int;
  y : int;
}
[@@deriving yojson]

type customizations_t = {
  color : string;
  head : string;
  tail : string;
}
[@@deriving yojson]

type snake_t = {
  id : string;
  name : string;
  health : int;
  body : coord_t list;
  head : coord_t;
  length : int;
  latency : string option; [@yojson.option]
  shout : string option; [@yojson.option]
  squad : string option; [@yojson.option]
  customizations : customizations_t option; [@yojson.option]
}
[@@deriving yojson]

type board_t = {
  height : int;
  width : int;
  snakes : snake_t list;
  food : coord_t list;
  hazards : coord_t list option; [@yojson.option]
}
[@@deriving yojson]

type game_state_t = {
  game : game_t;
  turn : int;
  board : board_t;
  you : snake_t;
}
[@@deriving yojson]

type move_response_t = { move : string } [@@deriving yojson]

type direction =
  | Up
  | Down
  | Left
  | Right

type move_t = {
  mutable up : bool;
  mutable down : bool;
  mutable left : bool;
  mutable right : bool;
}

let list_of_moves (m : move_t) =
  [ ("up", m.up); ("down", m.down); ("left", m.left); ("right", m.right) ]

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
  TODO: Step 1 - Prevent your Battlesnake from moving out of bounds
  let board_width = game_state.board.width in
  let board_height = game_state.board.height in
*)

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
