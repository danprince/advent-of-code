# http://adventofcode.com/2017/day/16

def spin(programs, n)
  (programs.pop n) + programs
end

def exchange(programs, a, b)
  programs.swap a, b
end

def partner(programs, x, y)
  a = programs.index x
  b = programs.index y
  a && b ? programs.swap a, b : programs
end

def parse_move(str)
  name = str[0]
  args = str[1..str.size].split '/'
  {name, args}
end

def dance(programs, moves)
  moves.reduce programs do |programs, move|
    name, args = parse_move move

    case name
      when 's'
        spin programs, args[0].to_i
      when 'x'
        exchange programs, args[0].to_i, args[1].to_i
      when 'p'
        partner programs, args[0], args[1]
      else
        programs
    end
  end
end

def solve(input)
  moves = input.split ","
  programs = "abcdefghijklmnop".split ""
  dance programs, moves
end
