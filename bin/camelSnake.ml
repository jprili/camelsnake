open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type info_response = {
  apiversion : string;
  author : string;
  color : string;
  head  : string;
  tail  : string;
} [@@deriving yojson]

(** As specified in the API docs *)
type settings_t = {
  foodSpawnChance: int;
  minimumFood: int
} [@@deriving yojson]

type ruleset_t = {
  name: string;
  settings: settings_t;
} [@@deriving yojson]

type game_t = {
  id: string;
  ruleset: ruleset_t;
  map: string;
  timeout: int;
} [@@deriving yojson]

type coord_t = {
  x: int;
  y: int;
} [@@deriving yojson]

type snake_t = {
  id: string;
  name: string;
  health: int;
  body: coord_t list;
  head: coord_t;
  length: int;
} [@@deriving yojson]

type board_t = {
  height: int;
  width:  int;
  snakes: snake_t list;
  food: coord_t list;
} [@@deriving yojson]

type game_state_t = {
  game: game_t;
  turn: int;
  board: board_t;
  you: snake_t;
} [@@deriving yojson]

let info _ = 
  {
    apiversion = "1";
    author = "";
    color = "#c0ffee";
    head = "default";
    tail = "default";
  }
  |> yojson_of_info_response
  |> Yojson.Safe.to_string
  |> Dream.json
;;

let on_start _ = 
  (**
  let%lwt body = Dream.body request in
  let game_state = body 
    |> Yojson.Safe.from_string
    |> game_state_t_of_yojson
  in
  *)
  Dream.log "GAME START"; 
  "ok" |> Dream.json
;;

let on_end _ = 
  (**
  let%lwt body = Dream.body request in
  let game_state = body 
    |> Yojson.Safe.from_string
    |> game_state_t_of_yojson
  in
  *)
  Dream.log "GAME OVER\n"; 
  "ok" |> Dream.json
;;
(* let on_move _ = None;; *)
