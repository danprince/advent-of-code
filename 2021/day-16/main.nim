import std/[strutils, sequtils]

type
  Parser = ref object
    data: string
    cursor: int

  Packet = object
    version: int
    typeId: range[0 .. 7]
    value: int
    subpackets: seq[Packet]

func hexToBin(s: string): string =
  for c in s:
    let num = parseHexInt($c)
    let bin = num.toBin(4)
    result &= bin

func newParser(src: string): Parser =
  return Parser(data: hexToBin(src), cursor: 0)

func peek(parser: var Parser, len: int): string =
  parser.data[parser.cursor ..< parser.cursor + len]

func read(parser: var Parser, len: int): string =
  result = parser.peek(len)
  parser.cursor += len

func readInt(parser: var Parser, len: int): int =
  parser.read(len).parseBinInt

func readPacket(parser: var Parser): Packet =
  result.version = parser.readInt(3)
  result.typeId = parser.readInt(3)

  if result.typeId == 4:
    var literal = ""
    while true:
      let more = parser.read(1)
      literal &= parser.read(4)
      if more == "0": break
    result.value = literal.parseBinInt
    return result

  let lengthTypeId = parser.read(1) 

  if lengthTypeId == "0":
    let totalBitLength = parser.readInt(15)
    let finalCursor = parser.cursor + totalBitLength - 1
    while parser.cursor <= finalCursor:
      result.subpackets.add(parser.readPacket)

  else:
    let totalPacketCount = parser.readInt(11)
    for i in 1 .. totalPacketCount:
      result.subpackets.add(parser.readPacket)

func sumVersions(packet: Packet): int =
  result += packet.version
  for sp in packet.subpackets:
    result += sumVersions(sp)

func add(xs: seq[int]): int =
  for x in xs: result += x

func mul(xs: seq[int]): int =
  result = 1; for x in xs: result *= x

func evaluate(packet: Packet): int =
  let vals = packet.subpackets.map(evaluate)
  case packet.typeId:
    of 0: add(vals)
    of 1: mul(vals)
    of 2: min(vals)
    of 3: max(vals)
    of 4: packet.value
    of 5: int(vals[0] > vals[1])
    of 6: int(vals[0] < vals[1])
    of 7: int(vals[0] == vals[1])

func part1(input: string): int =
  var parser = newParser(input)
  parser.readPacket().sumVersions()

func part2(input: string): int =
  var parser = newParser(input)
  parser.readPacket().evaluate()

when isMainModule:
  assert part1("8A004A801A8002F478") == 16
  assert part1("620080001611562C8802118E34") == 12
  assert part1("C0015000016115A2E0802F182340") == 23
  assert part1("A0016C880162017C3686B18A3D4780") == 31

  assert part2("C200B40A82") == 3
  assert part2("04005AC33890") == 54
  assert part2("880086C3E88112") == 7
  assert part2("CE00C43D881120") == 9
  assert part2("D8005AC2A8F0") == 1
  assert part2("F600BC2D8F") == 0
  assert part2("9C005AC2F8F0") == 0
  assert part2("9C0141080250320F1802104A08") == 1

  const input = slurp("input.txt")
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)
