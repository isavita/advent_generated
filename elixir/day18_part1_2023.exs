
defmodule Day18 do
  def solve(input) do
    input
    |> parse_input()
    |> calculate_polygon_area()
  end

  defp parse_input(input) do
    Enum.reduce(input, {[{0, 0}], {0, 0}}, fn line, {vertices, current} ->
      [dir, len_str | _] = String.split(line, " ")
      len = String.to_integer(len_str)
      new_current = move(current, dir, len)
      {[new_current | vertices], new_current}
    end)
    |> elem(0)
    |> Enum.reverse()
  end

  defp move({x, y}, "U", len), do: {x, y - len}
  defp move({x, y}, "D", len), do: {x, y + len}
  defp move({x, y}, "L", len), do: {x - len, y}
  defp move({x, y}, "R", len), do: {x + len, y}

  defp calculate_polygon_area(vertices) do
    shoelace(vertices) + div(perimeter(vertices), 2) + 1
  end

  defp shoelace(vertices) do
    n = length(vertices)
    Enum.reduce(0..n - 1, 0, fn i, area ->
      next = rem(i + 1, n)
      {x1, y1} = Enum.at(vertices, i)
      {x2, y2} = Enum.at(vertices, next)
      area + x1 * y2 - y1 * x2
    end)
    |> abs()
    |> div(2)
  end

  defp perimeter(vertices) do
    n = length(vertices)
    Enum.reduce(0..n - 1, 0, fn i, perim ->
      next = rem(i + 1, n)
      {x1, y1} = Enum.at(vertices, i)
      {x2, y2} = Enum.at(vertices, next)
      perim + abs(x1 - x2) + abs(y1 - y2)
    end)
  end

  def read_file(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
  end
end

input = Day18.read_file("input.txt")
IO.puts(Day18.solve(input))
