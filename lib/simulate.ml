open Types
open Collision
open Territory

type state_tree = {
  board : board;
  score : int;
  your_move : direction option;
  children : state_tree list;
}

let lose_score = -10_000

let move_snake (snake : snake) dir : snake =
  let no_tail = snake.body |> without_tail |> List.rev in
  let new_head = move snake.head dir in
  { snake with head = new_head; body = new_head :: no_tail }

let simulate_next snakes moves =
  List.map
    (fun (snake, dir) -> move_snake snake dir)
    (List.combine snakes moves)

(** Cartesian product: one direction chosen from each snake's list. *)
let rec product = function
  | [] -> [ [] ]
  | dirs :: rest ->
      let tails = product rest in
      List.concat_map (fun d -> List.map (fun t -> d :: t) tails) dirs

let eliminate (board : board) =
  let in_bounds (c : coord) =
    c.x >= 0 && c.x < board.width && c.y >= 0 && c.y < board.height
  in
  let tails =
    List.concat_map
      (fun (s : snake) ->
        match s.body with
        | [] -> []
        | _ :: rest -> rest)
      board.snakes
  in
  let survives (s : snake) =
    in_bounds s.head
    && (not (List.mem s.head tails))
    && not
         (List.exists
            (fun (o : snake) ->
              o.id <> s.id
              && o.head = s.head
              && List.length o.body >= List.length s.body)
            board.snakes)
  in
  { board with snakes = List.filter survives board.snakes }

let rec index_of id i = function
  | [] -> -1
  | (s : snake) :: rest ->
      if s.id = id then
        i
      else
        index_of id (i + 1) rest

let get_next_states you_id board =
  let snakes = board.snakes in
  let blocked = current_hazards board in
  let dirs_for (s : snake) =
    match
      List.filter
        (fun d -> is_safe board.width board.height blocked (move s.head d))
        all_directions
    with
    | [] -> [ Up ]
    | l -> l
  in
  let you_idx = index_of you_id 0 snakes in
  product (List.map dirs_for snakes)
  |> List.map (fun combo ->
      ( List.nth combo you_idx,
        eliminate { board with snakes = simulate_next snakes combo } ))

let eval board you_id =
  let territory = define_territories board in
  let score = ref 0 in
  Array.iter
    (Array.iter (function
      | Owned { snake_id; _ } ->
          if snake_id = you_id then
            incr score
          else
            decr score
      | Blocked | Unclaimed -> ()))
    territory;
  !score

let rec build_state_tree you_id depth your_move board =
  let alive = List.exists (fun (s : snake) -> s.id = you_id) board.snakes in
  if not alive then
    { board; score = lose_score - depth; your_move; children = [] }
  else
    let score = eval board you_id in
    if depth <= 0 then
      { board; score; your_move; children = [] }
    else
      {
        board;
        score;
        your_move;
        children =
          get_next_states you_id board
          |> List.map (fun (mv, b) ->
              build_state_tree you_id (depth - 1) (Some mv) b);
      }

let rec value node =
  match best_of node.children with
  | None -> node.score
  | Some (_, v) -> v

and best_of children =
  let dirs =
    children |> List.filter_map (fun c -> c.your_move) |> List.sort_uniq compare
  in
  List.fold_left
    (fun best d ->
      let worst =
        children
        |> List.filter (fun c -> c.your_move = Some d)
        |> List.fold_left (fun m c -> min m (value c)) max_int
      in
      match best with
      | Some (_, bv) when bv >= worst -> best
      | _ -> Some (d, worst))
    None dirs

let choose_move board you_id depth =
  let root = build_state_tree you_id depth None board in
  match best_of root.children with
  | Some (dir, _) -> Some dir
  | None -> None
