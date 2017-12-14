# http://adventofcode.com/2017/day/6

def reallocate(blocks)
  index = blocks.index blocks.max
  value = blocks[index]
  blocks[index] = 0

  while value > 0 do
    index = (index + 1) % blocks.length
    blocks[index] += 1
    value -= 1
  end

  blocks
end

def solve(blocks)
  seen = {}
  cycles = 0

  while seen[blocks] == nil do
    seen[blocks] = true
    blocks = reallocate blocks
    cycles += 1
  end

  cycles
end
