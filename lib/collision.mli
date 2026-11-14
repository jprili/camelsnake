open Types
open CoordSet

val get_hazards : board_t -> string -> CoordSet.t
val get_safe_moves : board_t -> CoordSet.t -> snake_t -> string array
