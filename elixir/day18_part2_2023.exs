
defmodule Solution do
  def solve(input) do
    input
    |> parse_input()
    |> calculate_polygon_area()
  end

  defp parse_input(input) do
    Enum.reduce(input, [{0, 0}], fn line, acc ->
      [_, _, color] = String.split(line, " ")
      dir = String.at(color, 7)
      length = String.slice(color, 2..6) |> String.to_integer(16)
      current = List.last(acc)
      next = move(current, dir, length)
      acc ++ [next]
    end)
  end

  defp move({x, y}, "3", length), do: {x, y - length}
  defp move({x, y}, "2", length), do: {x - length, y}
  defp move({x, y}, "1", length), do: {x, y + length}
  defp move({x, y}, "0", length), do: {x + length, y}

  defp calculate_polygon_area(vertices) do
    shoelace(vertices) + perimeter(vertices) / 2 + 1
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
end

input = File.read!("input.txt") |> String.split("\n", trim: true)
Solution.solve(input) |> IO.puts()
