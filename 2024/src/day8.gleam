import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import simplifile.{read}

type Point {
  Point(x: Int, y: Int)
}

type AntennaMap =
  Dict(String, List(Point))

type City =
  Dict(Point, String)

const example = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

pub fn main() {
  let assert Ok(input) = read("./input/day8.txt")

  let assert 14 = part1(example)
  let assert 252 = part1(input)

  let assert 34 = part2(example)
  let assert 839 = part2(input)
}

fn part1(input: String) -> Int {
  let city = parse_city(input)

  city
  |> build_antenna_map
  |> dict.values
  |> list.flat_map(list.combination_pairs)
  |> list.flat_map(find_antinodes(_, city))
  |> set.from_list
  |> set.size
}

fn part2(input: String) -> Int {
  let city = parse_city(input)

  city
  |> build_antenna_map
  |> dict.values
  |> list.flat_map(list.combination_pairs)
  |> list.flat_map(find_antinodes_with_resonant_harmonics(_, city))
  |> set.from_list
  |> set.size
}

fn parse_city(input: String) -> City {
  input
  |> string.trim
  |> string.split("\n")
  |> list.index_map(fn(line, y) {
    let chars = string.split(line, "")
    list.index_map(chars, fn(char, x) { #(Point(x, y), char) })
  })
  |> list.flatten
  |> dict.from_list
}

fn build_antenna_map(city: City) -> AntennaMap {
  dict.fold(city, dict.new(), fn(map, point, char) {
    case char {
      "." -> map
      _ -> {
        let points = dict.get(map, char) |> result.unwrap([])
        dict.insert(map, char, [point, ..points])
      }
    }
  })
}

fn find_antinodes(pairing: #(Point, Point), city: City) -> List(Point) {
  let #(a, b) = pairing
  let dx = b.x - a.x
  let dy = b.y - a.y
  let points = [Point(a.x - dx, a.y - dy), Point(b.x + dx, b.y + dy)]
  points |> list.filter(dict.has_key(city, _))
}

fn find_antinodes_with_resonant_harmonics(
  pairing: #(Point, Point),
  city: City,
) -> List(Point) {
  let #(a, b) = pairing
  let delta = Point(b.x - a.x, b.y - a.y)
  let inverse_delta = Point(a.x - b.x, a.y - b.y)
  list.flatten([
    find_harmonics(a, delta, city),
    find_harmonics(b, inverse_delta, city),
  ])
}

fn find_harmonics(point: Point, delta: Point, city: City) -> List(Point) {
  let harmonic = Point(point.x + delta.x, point.y + delta.y)
  case dict.has_key(city, point) {
    False -> []
    True -> [point, ..find_harmonics(harmonic, delta, city)]
  }
}
