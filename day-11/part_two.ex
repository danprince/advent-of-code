# http://adventofcode.com/2017/day/11

defmodule PartTwo do
  def apply_direction(direction, {x, y}) do
    case direction do
      :n   -> {x, y - 1};
      :ne  -> {x + 1, y};
      :se  -> {x + 1, y + 1};
      :s   -> {x, y + 1};
      :sw  -> {x - 1, y};
      :nw  -> {x - 1, y - 1};
      Else -> {x, y}
    end
  end

  def to_path(start_at, directions) do
    Enum.reduce directions, [start_at], fn(direction, [position | positions]) ->
      new_position = PartOne.apply_direction direction, position
      [new_position, position | positions]
    end
  end

  def solve(directions) do
    start = { 0, 0 }
    path = to_path start, directions
    Enum.max for point <- path, do: PartOne.distance point, start
  end
end
