// http://adventofcode.com/2017/day/10

package advent.part.two

import advent.part.one.{State, knot}

def xor(x: Int, y: Int): Int = x ^ y

def toHex(x: Int): String = {
  val s = x.toHexString

  s.length match {
    case 1 => "0" ++ s
    case _ => s
  }
}

def compact(items: Array[Int]): String = {
  val nums = for {
    block <- items.sliding(16, 16)
    number = block.reduce(xor)
    hex = toHex(number)
  } yield hex

  nums.mkString("")
}

def solve(input: String): String = {
  val lengths = input.toList.map { x => x.toInt } ++ List(17, 31, 73, 47, 23)
  val items = Array.range(0, 256)
  var state = State(items, 0, 0)

  for (x <- 0 until 64) {
    state = lengths.foldLeft(state)(knot)
  }

  compact(state.items)
}

