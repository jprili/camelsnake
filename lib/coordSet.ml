open Types

module Coord = struct
  type t = coord

  let compare a b =
    match Int.compare a.x b.x with
    | 0 -> Int.compare a.y b.y
    | c -> c
end

module MakeExtendedSet (Ord : Set.OrderedType) = struct
  module S = Set.Make (Ord)
  include S

  let union_list = List.fold_left union empty
end

module CoordSet = MakeExtendedSet (Coord)
