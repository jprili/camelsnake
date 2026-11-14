type coord = { x: int; y: int }

module type FloodFill = sig

  val flood_fill : (int * int) list -> coord -> int -> int -> int
end

let flood_fill hazards {x; y} w h =
  let visit = Array.make_matrix w h false in

  let rec count x y =
    if List.mem (x, y) hazards then 0
    else (
      visit.(x).(y) <- true;
      1
      + count (x - 1) y
      + count (x + 1) y
      + count x (y - 1)
      + count x (y + 1)
    )
  in
  count x y
