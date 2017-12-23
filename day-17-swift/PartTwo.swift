// http://adventofcode.com/2017/day/17

func solve(steps: Int) -> Int {
  var index = 0
  var length = 1
  var target = 0

  for value in 1...50_000_000 {
    index = (index + steps) % length + 1
    length += 1

    if index == 1 {
      target = value
    }
  }

  return target
}
