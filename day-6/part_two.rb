# http://adventofcode.com/2017/day/6

require "./part_one"

def solve(blocks)
  seen = {}
  cycles = 0

  while seen[blocks] == nil do
    seen[blocks] = cycles
    blocks = reallocate blocks
    cycles += 1
  end

  cycles - seen[blocks]
end
