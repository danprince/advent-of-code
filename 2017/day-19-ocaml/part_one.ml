open Printf

type state =
  { direction: int;
    cursor: int * int;
    path: string list;
    steps: int;
  }

let directions = [| (-1, 0); (0, -1); (1, 0); (0, 1) |]

let parse_row input =
  let row = Str.split (Str.regexp "") input in
  Array.of_list row

let parse_grid input =
  let lines = Str.split (Str.regexp "\n") input in
  let grid = List.map parse_row lines in
  Array.of_list grid

let find_start input =
  (String.index input '|', 0)

let find_direction grid state =
  let (d1, d2) = (state.direction + 1, state.direction + 3) in
  let cursor = move state.cursor d1 in
  let cell = get_node grid cursor in
  if cell <> " " then d1 else d2

let get_direction n =
  let length = Array.length directions in
  directions.(n mod length)

let get_node grid (x, y) =
  try grid.(y).(x) with | _ -> " "

let move cursor direction =
  let (x, y) = cursor in
  let (mx, my) = get_direction direction in
  (x + mx, y + my)

let add_to_path state letter =
  { state with path = letter::state.path }

let take_step state direction =
  { state with
    direction = direction ;
    cursor = move state.cursor direction ;
    steps = state.steps + 1 }

let rec walk grid state =
  let node = get_node grid state.cursor in
  let next state direction = walk grid (take_step state direction) in
  match node with
  | "|" -> next state state.direction
  | "-" -> next state state.direction
  | "+" -> next state (find_direction grid state)
  | " " -> { state with path = List.rev state.path }
  | letter -> next (add_to_path state letter) state.direction

let solve input =
  let grid = parse_grid input in
  let start = find_start input in
  let ending = walk grid {
    direction = 3;
    cursor = start;
    path = [];
    steps = 0;
  } in ending.path
