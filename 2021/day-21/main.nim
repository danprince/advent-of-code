import std/[strscans]

type
  Player = ref object
    pos: int
    score: int
  Game = object
    die: int
    rolls: int
    p1: Player
    p2: Player

proc parse(input: string): Game =
  let (ok, a, b) = scanTuple(input, "Player 1 starting position: $i\nPlayer 2 starting position: $i")
  assert ok, "Could not parse input"
  result.p1 = Player(pos: a, score: 0)
  result.p2 = Player(pos: b, score: 0)

proc roll(game: var Game, times: int = 1): int =
  for i in 1 .. times:
    game.die = (game.die + 1) mod 100
    game.rolls += 1
    result += game.die

proc part1(input: string): int =
  var game = parse(input)
  var p1 = game.p1
  var p2 = game.p2

  while true:
    p1.pos = ((p1.pos + game.roll(3) - 1) mod 10) + 1
    p1.score += p1.pos
    if p1.score >= 1000:
      return p2.score * game.rolls

    p2.pos = ((p2.pos + game.roll(3) - 1) mod 10) + 1
    p2.score += p2.pos
    if p2.score >= 1000:
      return p1.score * game.rolls

proc part2(input: string): int =
  let game = parse(input)
  var stack = @[game]



when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 739785
  assert part2(example) == 0

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
