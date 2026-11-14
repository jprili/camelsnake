open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Yojson.Basic.Util
open Camelsnake.Types
open Camelsnake.Collision
open Camelsnake.Fill

(* region CALLBACK_HANDLERS *)
let handle_move { board; you } =
  let hazards = get_hazards board you.id in
  let safe_moves = get_safe_moves board hazards you in

  (* TODO: smart moves, for now just move randomly *)
  let selected_move =
    match safe_moves |> Array.length with
    | 0 -> "up"
    | length -> length |> Random.int |> Array.get safe_moves
  in
  [%yojson_of: move_response] { move = selected_move }
  |> Yojson.Safe.to_string
(* endregion *)

(* region CALLBACKS *)
let on_move request =
  let%lwt body = Dream.body request in
  Dream.log "%s\n" body;
  body
  |> Yojson.Safe.from_string
  |> game_state_of_yojson
  |> handle_move
  |> Dream.json

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
  Dream.log "GAME START";
  "ok" |> Dream.json

let on_end _ =
  Dream.log "GAME OVER\n";
  "ok" |> Dream.json

(* endregion *)
