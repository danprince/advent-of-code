import std/[re, sugar, sequtils, strutils, intsets, strformat, terminal]

const BoardSize = 5

type
  Board = seq[int]
  Game = object
    boards: seq[Board]
    drawNumbers: seq[int]
    calledNumbers: seq[int]

proc parseGame(str: string): Game =
  let blocks = str.split("\n\n")
  result.drawNumbers = blocks[0].split(re",").map(parseInt)
  result.boards = collect:
    for b in blocks[1 .. ^1]:
      b.strip.split(re"\s+").map(parseInt).Board

proc drawNextNumber(game: var Game) =
  let num = game.drawNumbers[0]
  game.drawNumbers.delete(0)
  game.calledNumbers.add(num)

proc debug(game: var Game, board: Board) =
  for y in 0 ..< BoardSize:
    for x in 0 ..< BoardSize:
      let n = board[x + y * BoardSize]
      if n in game.calledNumbers:
        stdout.styledWrite(fgWhite, fmt"{n:>2} ")
      else:
        stdout.styledWrite(fgBlack, fmt"{n:>2} ", styleItalic)
    stdout.write "\n"

  let calls = game.calledNumbers.join(" ")
  stdout.styledWrite(fgBlack, fmt"calls: {calls}", styleItalic)
  stdout.write "\n\n"

proc hasWinState(game: var Game, board: Board): bool =
  if game.calledNumbers.len < BoardSize:
    return false;

  let calls = game.calledNumbers.toIntSet

  # rows
  for y in 0 ..< BoardSize:
    var allMarked = true
    for x in 0 ..< BoardSize:
      if board[x + y * BoardSize] notin calls:
        allMarked = false
        break
    if allMarked:
      return true

  # columns
  for x in 0 ..< BoardSize:
    var allMarked = true
    for y in 0 ..< BoardSize:
      if board[x + y * BoardSize] notin calls:
        allMarked = false
        break
    if allMarked:
      return true

  return false

proc score(game: var Game, board: Board): int =
  let numbers = board.toIntSet
  let calledNumbers = game.calledNumbers.toIntSet
  let unmarkedNumbers = numbers.difference(calledNumbers)
  let mostRecentCall = game.calledNumbers[^1]
  for n in unmarkedNumbers: result += n
  result *= mostRecentCall

proc part1(input: string): int =
  var game = parseGame(input)
  while game.drawNumbers.len > 0:
    game.drawNextNumber
    for board in game.boards:
      if game.hasWinState(board):
        return game.score(board)

proc part2(input: string): int =
  var game = parseGame(input)
  while game.drawNumbers.len > 0:
    game.drawNextNumber
    for i, board in game.boards:
      if game.hasWinState(board):
        if game.boards.len == 1:
          return game.score(board)

    game.boards = game.boards.filterIt(not game.hasWinState(it))

when isMainModule:
  const example = slurp("example.txt")
  assert part1(example) == 4512
  assert part2(example) == 1924
  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
