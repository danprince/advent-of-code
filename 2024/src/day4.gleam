import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import simplifile

type Point =
  #(Int, Int)

type Grid =
  Dict(Point, String)

const directions = [
  #(-1, -1), #(0, -1), #(1, -1), #(-1, 0), #(1, 0), #(-1, 1), #(0, 1), #(1, 1),
]

const example = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

pub fn main() {
  let assert Ok(input) = simplifile.read("./input/day4.txt")

  let grid = parse("ABC\nDEF\nGHI")
  let assert Ok("A") = dict.get(grid, #(0, 0))
  let assert Ok("F") = dict.get(grid, #(2, 1))
  let assert Error(Nil) = dict.get(grid, #(5, 2))

  let word = "XMAS"
  let assert 18 = part1(example, word)
  let assert 2507 = part1(input, word)

  let word = "MAS"
  let assert 9 = part2(example, word)
  let assert 1969 = part2(input, word)
}

fn part1(input: String, word: String) -> Int {
  let grid = parse(input)
  let points = dict.keys(grid)
  list.fold(points, 0, fn(score, point) {
    score + list.count(directions, word_search(grid, point, word, _))
  })
}

fn part2(input: String, word: String) -> Int {
  let grid = parse(input)
  let points = dict.keys(grid)
  list.count(points, x_search(grid, _, word))
}

fn word_search(grid: Grid, point: Point, word: String, direction: Point) -> Bool {
  let #(x, y) = point
  let #(dx, dy) = direction
  let next_point = #(x + dx, y + dy)

  let assert Ok(#(search_letter, rest_of_word)) = string.pop_grapheme(word)

  case dict.get(grid, point) {
    Ok(letter) -> {
      case letter == search_letter, rest_of_word {
        False, _ -> False
        True, "" -> True
        True, _ -> word_search(grid, next_point, rest_of_word, direction)
      }
    }

    Error(_) -> False
  }
}

fn x_search(grid: Grid, point: Point, word: String) -> Bool {
  let #(x, y) = point

  let center = dict.get(grid, #(x, y)) |> result.unwrap("")
  let top_left = dict.get(grid, #(x - 1, y - 1)) |> result.unwrap("")
  let top_right = dict.get(grid, #(x + 1, y - 1)) |> result.unwrap("")
  let bottom_left = dict.get(grid, #(x - 1, y + 1)) |> result.unwrap("")
  let bottom_right = dict.get(grid, #(x + 1, y + 1)) |> result.unwrap("")

  let assert [m, a, s] = string.to_graphemes(word)
  let d1 = top_left == m && center == a && bottom_right == s
  let d2 = top_left == s && center == a && bottom_right == m
  let d3 = top_right == m && center == a && bottom_left == s
  let d4 = top_right == s && center == a && bottom_left == m

  { d1 || d2 } && { d3 || d4 }
}

fn parse(input: String) -> Grid {
  input
  |> string.trim
  |> string.split("\n")
  |> list.index_map(fn(line, y) {
    let graphemes = string.to_graphemes(line)
    list.index_map(graphemes, fn(grapheme, x) {
      let point: Point = #(x, y)
      #(point, grapheme)
    })
  })
  |> list.flatten
  |> dict.from_list
}
