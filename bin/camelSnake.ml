open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Yojson.Basic.Util
open Camelsnake.Types
open Camelsnake.Collision
open Camelsnake.Fill
open Camelsnake.Food

let current_direction you =
  match you.body with
  | head :: prev_head :: _ -> get_direction prev_head head
  | _ -> Up

let direction_name = function
  | Up -> "up"
  | Down -> "down"
  | Left -> "left"
  | Right -> "right"

(* region CALLBACK_HANDLERS *)
let handle_move { board; you } =
  let { width; height } = board in
  let hazards = get_hazards board you.id in
  let territories = define_territories board in

  let safe_moves = get_safe_moves board hazards you in
  let best_directions =
    flood_fill_prune width height hazards safe_moves
  in

  let best_moves =
    List.filter
      (fun (direction, _) -> List.mem direction best_directions)
      safe_moves
  in

  let territory_preferred_moves =
    List.filter
      (fun (_, coord) ->
        match territories.(coord.x).(coord.y) with
        | Owned { snake_id; _ } -> snake_id = you.id
        | Blocked | Unclaimed -> false)
      best_moves
  in

  let choices =
    if territory_preferred_moves = [] then
      best_moves
    else
      territory_preferred_moves
  in

  let selected_move =
    match choices with
    | [] -> you |> current_direction |> direction_name
    | moves ->
        let direction, _ =
          List.nth moves (Random.int (List.length moves))
        in
        direction_name direction
  in

  [%yojson_of: move_response] { move = selected_move }
  |> Yojson.Safe.to_string
(* endregion *)

(* region CALLBACKS *)
let on_move request =
  let%lwt body = Dream.body request in
  (* Dream.log "%s\n" body; *)
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
