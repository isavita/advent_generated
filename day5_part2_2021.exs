
defmodule HydrothermalVenture do
  def call do
    input = File.read!("input.txt")
    lines = parse_input(input)
    {part1, part2} = Enum.reduce(lines, {%{}, %{}}, fn {x1, y1, x2, y2}, {acc1, acc2} ->
      acc1 = update_map(acc1, line_points(x1, y1, x2, y2, false))
      acc2 = update_map(acc2, line_points(x1, y1, x2, y2, true))
      {acc1, acc2}
    end)
    {count_overlaps(part1), count_overlaps(part2)}
  end

  defp parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    [start, _arrow, finish] = String.split(line, " ")
    [x1, y1] = String.split(start, ",") |> Enum.map(&String.to_integer/1)
    [x2, y2] = String.split(finish, ",") |> Enum.map(&String.to_integer/1)
    {x1, y1, x2, y2}
  end

  defp line_points(x1, y1, x2, y2, include_diagonals) do
    cond do
      x1 == x2 -> Enum.map(min(y1, y2)..max(y1, y2), fn y -> {x1, y} end)
      y1 == y2 -> Enum.map(min(x1, x2)..max(x1, x2), fn x -> {x, y1} end)
      include_diagonals && abs(x1 - x2) == abs(y1 - y2) -> diagonal_points(x1, y1, x2, y2)
      true -> []
    end
  end

  defp diagonal_points(x1, y1, x2, y2) do
    dx = if x2 > x1, do: 1, else: -1
    dy = if y2 > y1, do: 1, else: -1
    Enum.map(0..abs(x1-x2), fn i -> {x1 + i*dx, y1 + i*dy} end)
  end

  defp update_map(map, points) do
    Enum.reduce(points, map, fn point, acc ->
      Map.update(acc, point, 1, &(&1 + 1))
    end)
  end

  defp count_overlaps(map) do
    map
    |> Enum.count(fn {_k, v} -> v > 1 end)
  end
end
