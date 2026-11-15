open Types
open Collision
open CoordSet

let ( let* ) = Option.bind

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

  let compare a b =
    match Int.compare a.dist b.dist with
    | 0 -> Coord.compare a.coord b.coord
    | c -> c

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
      | Some old_node -> set |> NodeSet.remove old_node |> NodeSet.add node
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
    let* min_n = NodeSet.min_elt_opt set in
    Some (min_n, remove min_n current)
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

let find_food width height foods hazards head =
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
  let rec search current unvisited food res =
    let next_food, next_res =
      if CoordSet.mem current.coord food then
        (food |> CoordSet.remove current.coord, get_food_path current :: res)
      else
        (food, res)
    in
    if CoordSet.is_empty food then
      next_res
    else
      match
        neighbors current.coord
        |> List.map (fun coord ->
            { coord; dist = current.dist + 1; prev = Some current })
        |> List.fold_left NodePQ.update unvisited
        |> NodePQ.pop_min
      with
      | Some (next, next_unvisited) ->
          search next next_unvisited next_food next_res
      | None -> next_res
  in
  let origin = { coord = head; dist = 0; prev = None } in
  search origin NodePQ.empty (CoordSet.of_list foods) []
  |> List.fold_left
       (fun map path ->
         DirMap.update path.move
           (function
             | None -> Some path
             | Some p ->
                 Some
                   (if path.dist < p.dist then
                      path
                    else
                      p))
           map)
       DirMap.empty
  |> DirMap.bindings
