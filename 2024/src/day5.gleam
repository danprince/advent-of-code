import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/set
import gleam/string
import simplifile.{read}

const example = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

type Rule =
  #(Int, Int)

type Update =
  List(Int)

type PuzzleInput {
  PuzzleInput(rules: List(Rule), updates: List(Update))
}

pub fn main() {
  let assert Ok(input) = read("./input/day5.txt")

  let assert 143 = part1(example)
  let assert 6949 = part1(input)

  let assert 123 = part2(example)
  let assert 4145 = part2(input)
}

fn part1(input: String) -> Int {
  let assert Ok(PuzzleInput(rules, updates)) = parse(input)
  let correct_updates = list.filter(updates, is_correct(rules, _))
  let assert Ok(middle_values) = list.try_map(correct_updates, middle)
  list.fold(middle_values, 0, int.add)
}

fn part2(input: String) -> Int {
  let assert Ok(PuzzleInput(rules, updates)) = parse(input)
  let #(_, incorrect_updates) = list.partition(updates, is_correct(rules, _))
  let fixed_updates = list.map(incorrect_updates, fix_order(rules, _))
  let assert Ok(middle_values) = list.try_map(fixed_updates, middle)
  list.fold(middle_values, 0, int.add)
}

fn parse(input: String) -> Result(PuzzleInput, Nil) {
  let input = string.trim(input)
  let sections = string.split_once(input, "\n\n")
  let #(rules, updates) = result.unwrap(sections, #("", ""))
  let rules = string.split(rules, "\n") |> list.try_map(parse_rule)
  let updates = string.split(updates, "\n") |> list.try_map(parse_update)
  case rules, updates {
    Ok(rules), Ok(updates) -> Ok(PuzzleInput(rules, updates))
    _, _ -> Error(Nil)
  }
}

fn parse_rule(str: String) -> Result(Rule, Nil) {
  let parts = string.split(str, "|") |> list.try_map(int.parse)
  case parts {
    Ok([left, right]) -> Ok(#(left, right))
    _ -> Error(Nil)
  }
}

fn parse_update(str: String) -> Result(Update, Nil) {
  string.split(str, ",") |> list.try_map(int.parse)
}

fn is_correct(rules: List(Rule), update: Update) -> Bool {
  let seen =
    list.fold_until(update, set.new(), fn(seen, page) {
      let seen = set.insert(seen, page)
      let pages_after = list.key_filter(rules, page)
      let out_of_order = list.any(pages_after, set.contains(seen, _))
      case out_of_order {
        True -> list.Stop(set.new())
        False -> list.Continue(seen)
      }
    })
  !set.is_empty(seen)
}

fn middle(xs: List(a)) -> Result(a, Nil) {
  xs |> list.drop(list.length(xs) / 2) |> list.first
}

fn fix_order(rules: List(Rule), update: Update) -> Update {
  list.sort(update, fn(a, b) {
    let pages_after_a = list.key_filter(rules, a)
    let pages_after_b = list.key_filter(rules, b)
    let a_is_first = list.contains(pages_after_a, b)
    let b_is_first = list.contains(pages_after_b, a)
    case a_is_first, b_is_first {
      True, _ -> order.Gt
      False, True -> order.Lt
      False, False -> order.Eq
    }
  })
}
