open Types
open CoordSet

let flood_fill width height hazards coord =
  let visited = Array.make_matrix width height false in
  let rec count x y =
    if CoordSet.mem coord hazards || visited.(x).(y) then
      0
    else (
      visited.(x).(y) <- true;
      1 + count (x - 1) y + count (x + 1) y + count x (y - 1) + count x (y + 1))
  in
  count coord.x coord.y
