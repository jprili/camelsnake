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
