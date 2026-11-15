open Types
open Collision
open CoordSet

type node = {
  coord : coord;
  dist : int;
  prev : node option;
}

module Coord = struct
  type t = coord

  let compare a b =
    match Int.compare a.x b.x with
    | 0 -> Int.compare a.y b.y
    | c -> c
end

module Node = struct
  type t = node

  let compare (a : t) (b : t) = Int.compare a.dist b.dist

  let get_first_move from_coord =
    let rec aux = function
      | { coord; prev = Some prev } ->
          if prev.coord = from_coord then
            coord
          else
            aux prev
      | _ -> failwith "perhaps src = des? that's not allowed"
    in
    aux
end

module NodePQ = struct
  module CoordMap = Map.Make (Coord)
  module NodeSet = Set.Make (Node)

  type t = node CoordMap.t * NodeSet.t

  let upsert (map, set) node =
    let new_map = CoordMap.add node.coord node map in
    let new_set =
      match CoordMap.find_opt node.coord map with
      | Some old_node -> NodeSet.remove old_node set
      | None -> set |> NodeSet.add node
    in
    (new_map, new_set)

  let update ((map, set) as current) node =
    match CoordMap.find_opt node.coord map with
    | None -> upsert current node
    | Some old_node ->
        if node.dist < old_node.dist then
          upsert current node
        else
          current

  let empty = (CoordMap.empty, NodeSet.empty)

  let remove node (map, set) : t =
    let new_map = CoordMap.remove node.coord map in
    let new_set = NodeSet.remove node set in
    (new_map, new_set)

  let pop_min ((_, set) as current) =
    match NodeSet.min_elt_opt set with
    | Some min_n -> Some (min_n, remove min_n current)
    | None -> None
end

let get_direction src des =
  if src.x = des.x then
    if src.y < des.y then
      Up
    else
      Down
  else if src.x < des.x then
    Right
  else
    Left

type food_path = {
  move : direction;
  dist : int;
  food : coord;
}

module DirMap = Map.Make (struct
  type t = direction

  let compare = compare
end)

(** Return Map(direction => list of food reachable in that direction).

Each list item has type type {dist: distance to food; food: coord of food}
*)
let paths_to_food width height foods hazards head =
  let neighbors coord =
    all_directions
    |> List.map (move coord)
    |> List.filter (is_safe width height hazards)
  in
  let get_food_path food =
    {
      move = food |> Node.get_first_move head |> get_direction head;
      dist = food.dist;
      food = food.coord;
    }
  in
  let rec search (current : Node.t) unvisited food res =
    let updated_unvisited =
      if CoordSet.is_empty food then
        NodePQ.empty
      else if CoordSet.mem current.coord food then
        unvisited
      else
        neighbors current.coord
        |> List.map (fun coord ->
            { coord; dist = current.dist + 1; prev = Some current })
        |> List.fold_left NodePQ.update unvisited
    in
    match NodePQ.pop_min updated_unvisited with
    | Some (next, next_unvisited) ->
        search next next_unvisited
          (food |> CoordSet.remove current.coord)
          (get_food_path current :: res)
    | None -> res
  in
  let origin = { coord = head; dist = 0; prev = None } in
  let food_paths = search origin NodePQ.empty (CoordSet.of_list foods) [] in
  List.fold_left
    (fun map path ->
      DirMap.update path.move
        (function
          | None -> Some [ path ]
          | Some ps -> Some (path :: ps))
        map)
    DirMap.empty food_paths
