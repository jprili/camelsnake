open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Yojson.Basic.Util
open Camelsnake.Types
open Camelsnake.CoordSet
open Camelsnake.Collision
open Camelsnake.Fill
open Camelsnake.Food
open Camelsnake.Territory

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
  let potential_hazards = get_hazards board you.id in
  let territories = define_territories board in

  let safe_moves =
    get_safe_moves width height potential_hazards you.head
    |> flood_fill_prune width height potential_hazards
  in

  let best_moves =
    if List.length safe_moves <= 1 then
      safe_moves
    else
      let reachable_food =
        board.food
        |> List.filter (fun coord ->
            match territories.(coord.x).(coord.y) with
            | Owned { snake_id; _ } -> snake_id = you.id
            | _ -> false)
      in
      let food_moves =
        find_food width height reachable_food potential_hazards you.head
        |> List.filter (fun (dir, _) -> List.mem dir safe_moves)
      in
      let closest_food_dist =
        food_moves
        |> List.map (fun (_, path) -> path.dist)
        |> List.fold_left min max_int
      in
      let optimal_food_moves =
        food_moves
        |> List.filter (fun (_, path) -> path.dist = closest_food_dist)
        |> List.map (fun (dir, _) -> dir)
      in
      if List.length optimal_food_moves > 0 then
        optimal_food_moves
      else if List.length safe_moves > 0 then
        safe_moves
      else
        let definite_hazards =
          match board.hazards with
          | None -> CoordSet.empty
          | Some h -> CoordSet.of_list h
        in
        let definitely_safe_moves =
          get_safe_moves width height definite_hazards you.head
        in
        definitely_safe_moves |> List.map (fun (dir, _) -> dir)
  in

  let selected_move =
    (match best_moves with
      | [] -> you |> current_direction
      | moves -> moves |> List.length |> Random.int |> List.nth moves)
    |> direction_name
  in

  [%yojson_of: move_response] { move = selected_move } |> Yojson.Safe.to_string
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
