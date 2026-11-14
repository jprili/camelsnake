open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type info_response = {
  apiversion : string;
  author : string;
  color : string;
  head : string;
  tail : string;
}
[@@deriving yojson]

type royale = { shrinkEveryNTurns : int } [@@deriving yojson]
(** As specified in the API docs *)

type squad = {
  allowBodyCollisions : bool;
  sharedElimination : bool;
  sharedHealth : bool;
  sharedLength : bool;
}
[@@deriving yojson]

type settings = {
  foodSpawnChance : int;
  minimumFood : int;
  hazardDamagePerTurn : int option; [@yojson.option]
  hazardMap : string option; [@yojson.option]
  hazardMapAuthor : string option; [@yojson.option]
  royale : royale option; [@yojson.option]
  squad : squad option; [@yojson.option]
}
[@@deriving yojson]

type ruleset = {
  name : string;
  settings : settings;
  version : string option; [@yojson.option]
}
[@@deriving yojson]

type game = {
  id : string;
  ruleset : ruleset;
  map : string;
  timeout : int;
  source : string option; [@yojson.option]
}
[@@deriving yojson]

type coord = {
  x : int;
  y : int;
}
[@@deriving yojson]

type customizations = {
  color : string;
  head : string;
  tail : string;
}
[@@deriving yojson]

type snake = {
  id : string;
  name : string;
  health : int;
  body : coord list;
  head : coord;
  length : int;
  latency : string option; [@yojson.option]
  shout : string option; [@yojson.option]
  squad : string option; [@yojson.option]
  customizations : customizations option; [@yojson.option]
}
[@@deriving yojson]

type board = {
  height : int;
  width : int;
  snakes : snake list;
  food : coord list;
  hazards : coord list option; [@yojson.option]
}
[@@deriving yojson]

type game_state = {
  game : game;
  turn : int;
  board : board;
  you : snake;
}
[@@deriving yojson]

type move_response = { move : string } [@@deriving yojson]

type direction =
  | Up
  | Down
  | Left
  | Right
