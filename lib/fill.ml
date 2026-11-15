open Types
open CoordSet
open Collision

let flood_fill_prune width height hazards safe_moves =
  let flood_fill coord =
    let visited = Array.make_matrix width height false in
    let rec count x y =
      if (not (is_safe width height hazards { x; y })) || visited.(x).(y) then
        0
      else (
        visited.(x).(y) <- true;
        1 + count (x - 1) y + count (x + 1) y + count x (y - 1) + count x (y + 1))
    in
    count coord.x coord.y
  in
  let areas =
    List.map
      (fun (_, coord) ->
        (* Dream.log "%i %i" coord.x coord.y; *)
        flood_fill coord)
      safe_moves
  in
  (* Dream.log "%s" (List.map string_of_int areas |> String.concat "; "); *)
  let max_area = List.fold_left max 0 areas in
  List.filter_map
    (fun ((dir, _), area) ->
      if area = max_area then
        Some dir
      else
        None)
    (List.combine safe_moves areas)
