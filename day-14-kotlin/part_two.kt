// http://adventofcode.com/2017/day/14

package advent.day14

import kotlin.math.floor

typealias Point = Pair<Int, Int>

val width = 128
val height = 128

fun cartesian(index: Int): Point {
  val x = (index % width).toInt()
  val y = floor(index.toDouble() / width).toInt()
  return Pair(x, y)
}

fun countRegions(grid: List<Boolean>): Int {
  var regions = 0

  val points = (0 until grid.size)
    .filter { index -> grid[index] == true }
    .map { index -> cartesian(index) }
    .toMutableSet()

  while (!points.isEmpty()) {
    val point = points.first()
    removeRegion(points, point)
    regions += 1
  }

  return regions
}

fun removeRegion(points: MutableSet<Point>, point: Point) {
  val (x, y) = point

  val neighbours = listOf(
    Pair(x, y - 1),
    Pair(x - 1, y),
    Pair(x + 1, y),
    Pair(x, y + 1)
  );

  points.remove(point)

  for (neighbour in neighbours) {
    if (points.contains(neighbour)) {
      removeRegion(points, neighbour)
    }
  }
}

private fun solve(key: String) {
  val grid = generateGrid(key, height)
  return countRegions(grid)
}

