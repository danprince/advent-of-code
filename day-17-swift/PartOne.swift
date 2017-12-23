// http://adventofcode.com/2017/day/17

func solve(steps: Int) -> Int {
  var buffer = [0]
  var index = 0

  for value in 1...2017 {
    index = (index + steps) % buffer.count + 1
    buffer.insert(value, at: index)
  }

  return buffer[index + 1]
}
