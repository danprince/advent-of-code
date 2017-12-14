// http://adventofcode.com/2017/day/10

package advent.part.one

case class State( items : Array[Int] , position : Int, skip: Int )

def circular(index: Int, items: Array[Int]): Int = {
  index % items.length
}

def reverseThrough(items: Array[Int], pos: Int, len: Int): Array[Int] = {
  val through = for {
    index <- pos until pos + len
    real = circular(index, items)
  } yield real

  val lookups =
    (items.indices zip items.indices).toMap ++
    (through zip through.reverse).toMap

  val values = for {
    x <- items.indices
    y <- lookups get x
  } yield items(y)

  values.toArray
}

def knot(state: State, length: Int): State = {
  val items = reverseThrough(state.items, state.position, length)
  val position = state.position + state.skip + length
  val skip = state.skip + 1
  State(items, position, skip)
}

def solve(lengths: List[Int]): Int = {
  val items = Array.range(0, 256)
  val init = State(items, 0, 0)
  val result = lengths.foldLeft(init)(knot)
  result.items(0) * result.items(1)
}
