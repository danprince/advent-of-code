# http://adventofcode.com/2017/day/11

defmodule PartOne do
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

  def walk(start_at, directions) do
    Enum.reduce directions, start_at, &apply_direction/2
  end

  def distance({x1, y1}, {x2, y2}) do
    Enum.max [abs(x1 - x2), abs(y1 - y2)]
  end

  def solve(directions) do
    start_at = {0, 0}
    end_at = walk start_at, directions
    distance start_at, end_at
  end

  def load(file_name) do
    file_name
      |> File.read!
      |> String.trim
      |> String.split(",")
      |> Enum.map(&String.to_atom/1)
  end
end


