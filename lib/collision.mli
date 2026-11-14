open Types

val get_hazards : board_t -> string -> CoordSet.CoordSet.t
val get_safe_moves : board_t -> snake_t -> string array
