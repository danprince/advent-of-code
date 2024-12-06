import gleam/dict.{type Dict}
import gleam/list
import gleam/set.{type Set}
import gleam/string
import simplifile

const example = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

type Point =
  #(Int, Int)

type Grid =
  Dict(Point, String)

type Path =
  Set(#(Point, Direction))

type Direction {
  Up
  Right
  Down
  Left
}

pub fn main() {
  let assert Ok(input) = simplifile.read("./input/day6.txt")

  let assert 41 = part1(example)
  let assert 5212 = part1(input)

  let assert 6 = part2(example)
  // Brute force is too slow to leave this assertion in
  //let assert 1767 = part2(input)
}

fn part1(input: String) -> Int {
  let grid = parse_grid(input)
  let assert Ok(start) = find_in_grid(grid, "^")
  let grid = dict.insert(grid, start, ".")
  let assert Ok(path) = patrol(grid, start, Up, set.new())
  let visited = get_visited_points(path)
  list.length(visited)
}

fn part2(input: String) -> Int {
  let grid = parse_grid(input)
  let assert Ok(start) = find_in_grid(grid, "^")
  let grid = dict.insert(grid, start, ".")

  let points =
    dict.keys(grid) |> set.from_list |> set.delete(start) |> set.to_list

  list.count(points, fn(point) {
    let grid = dict.insert(grid, point, "#")
    let path = patrol(grid, start, Up, set.new())
    case path {
      Ok(_) -> False
      Error(_) -> True
    }
  })
}

fn find_in_grid(grid: Grid, search: String) -> Result(Point, Nil) {
  let points = dict.keys(grid)

  list.find(points, fn(point) {
    case dict.get(grid, point) {
      Ok(str) if str == search -> True
      _ -> False
    }
  })
}

fn parse_grid(input: String) -> Grid {
  input
  |> string.trim
  |> string.split("\n")
  |> list.index_map(fn(line, y) {
    let chars = string.split(line, "")
    list.index_map(chars, fn(char, x) { #(#(x, y), char) })
  })
  |> list.flatten
  |> dict.from_list
}

fn patrol(
  grid: Grid,
  point: Point,
  direction: Direction,
  path: Path,
) -> Result(Path, Path) {
  let #(x, y) = point

  let next_point = case direction {
    Up -> #(x, y - 1)
    Right -> #(x + 1, y)
    Down -> #(x, y + 1)
    Left -> #(x - 1, y)
  }

  let step = #(point, direction)
  let loop = set.contains(path, step)
  let path = set.insert(path, step)

  case loop, dict.get(grid, next_point) {
    True, _ -> Error(path)
    False, Ok(".") -> patrol(grid, next_point, direction, path)
    False, Ok("#") -> patrol(grid, point, turn(direction), path)
    False, _ -> Ok(path)
  }
}

fn get_visited_points(path: Path) -> List(Point) {
  path |> set.to_list |> dict.from_list |> dict.keys
}

fn turn(direction: Direction) -> Direction {
  case direction {
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up
  }
}
