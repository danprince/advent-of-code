# http://adventofcode.com/2017/day/16

require "./part_one"

def find_cycle(programs, moves)
  programs = programs.clone
  initial_programs = programs.clone
  iterations = 0

  loop do
    iterations += 1
    programs = dance programs, moves
    break if programs == initial_programs
  end

  iterations
end

def solve(input)
  moves = input.split ","
  programs = "abcdefghijklmnop".split ""
  cycle_length = find_cycle programs, moves
  limit = 1_000_000_000 % cycle_length
  iterations = 0

  while iterations < limit
    programs = dance programs, moves
    iterations += 1
  end

  return programs
end
