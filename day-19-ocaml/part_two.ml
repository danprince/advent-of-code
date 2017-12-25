#use "part_one.ml"

let solve input =
  let grid = parse_grid input in
  let start = find_start input in
  let ending = walk grid {
    direction = 3;
    cursor = start;
    path = [];
    steps = 0;
  } in ending.steps
