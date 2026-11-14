open Types
open CoordSet

val get_hazards : board -> string -> CoordSet.t
val get_safe_moves : board -> CoordSet.t -> snake -> string array
