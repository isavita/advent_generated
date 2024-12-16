
defmodule Tubes do
  def solve(input_path) do
    input = File.read!(input_path)
    grid = String.split(input, "\n") |> Enum.map(&String.graphemes/1)
    start_x = Enum.find_index(Enum.at(grid, 0), &(&1 != " "))
    start_y = 0
    walk(grid, {start_x, start_y}, :down, "", 0)
  end

  defp walk(grid, {x, y}, direction, letters, steps) do
    case get_char(grid, {x, y}) do
      " " -> {letters, steps}
      "+" ->
        next_direction = next_direction(grid, {x, y}, direction)
        walk(grid, move({x, y}, next_direction), next_direction, letters, steps + 1)
      char when char in ["|", "-"] ->
        walk(grid, move({x, y}, direction), direction, letters, steps + 1)
      char ->
        walk(grid, move({x, y}, direction), direction, letters <> char, steps + 1)
    end
  end

  defp get_char(grid, {x, y}) do
    case Enum.at(grid, y) do
      nil -> " "
      row ->
        case Enum.at(row, x) do
          nil -> " "
          char -> char
        end
    end
  end

  defp move({x, y}, :up), do: {x, y - 1}
  defp move({x, y}, :down), do: {x, y + 1}
  defp move({x, y}, :left), do: {x - 1, y}
  defp move({x, y}, :right), do: {x + 1, y}

  defp next_direction(grid, {x, y}, current_direction) do
    directions = [:up, :down, :left, :right]
    Enum.reject(directions, &(&1 == opposite_direction(current_direction)))
    |> Enum.find(fn direction ->
      get_char(grid, move({x, y}, direction)) != " "
    end)
  end

  defp opposite_direction(:up), do: :down
  defp opposite_direction(:down), do: :up
  defp opposite_direction(:left), do: :right
  defp opposite_direction(:right), do: :left
end

{letters, steps} = Tubes.solve("input.txt")
IO.puts("Letters: #{letters}")
IO.puts("Steps: #{steps}")
