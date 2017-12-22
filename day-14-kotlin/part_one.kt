// http://adventofcode.com/2017/day/14

package advent.day14

import advent.day10.PartTwo

fun numToBinary(n: Int): List<Boolean> {
  return listOf(
    n and 0b1000 == 0b1000,
    n and 0b0100 == 0b0100,
    n and 0b0010 == 0b0010,
    n and 0b0001 == 0b0001
  )
}

fun generateGrid(key: String, height: Int): List<Boolean> {
  val rows = (0 until height).map { index -> PartTwo.solve("$key-$index") }
  val chars = rows.joinToString("").toCharArray()

  return chars
    .map { chr -> Character.toString(chr) }
    .map { hex -> hex.toInt(16) }
    .flatMap { num -> numToBinary(num) }
}

private fun solve(key: String) {
  val grid = generateGrid(key, 127);
  val filled = grid.filter { it == true }
  return filled.size
}
