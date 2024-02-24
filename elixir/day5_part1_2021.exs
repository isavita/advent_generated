defmodule Day5 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    [start, stop] = String.split(line, " -> ")
    [x1, y1] = String.split(start, ",") |> Enum.map(&String.to_integer/1)
    [x2, y2] = String.split(stop, ",") |> Enum.map(&String.to_integer/1)
    {x1, y1, x2, y2}
  end

  defp points({x1, y1, x2, y2}) when x1 == x2 do
    Enum.map(min(y1, y2)..max(y1, y2), fn y -> {x1, y} end)
  end
  defp points({x1, y1, x2, y2}) when y1 == y2 do
    Enum.map(min(x1, x2)..max(x1, x2), fn x -> {x, y1} end)
  end
  defp points(_), do: []

  defp count_overlaps(lines) do
    lines
    |> Enum.flat_map(&points/1)
    |> Enum.reduce(%{}, fn point, acc ->
      Map.update(acc, point, 1, &(&1 + 1))
    end)
    |> Enum.count(fn {_point, count} -> count > 1 end)
  end

  def call do
    read_input()
    |> Enum.filter(fn {x1, y1, x2, y2} -> x1 == x2 or y1 == y2 end)
    |> count_overlaps()
    |> IO.puts()
  end
end

Day5.call()
