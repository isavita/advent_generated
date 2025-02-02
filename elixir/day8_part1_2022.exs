
defmodule Solution do
  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> then(fn lines ->
      for {line, y} <- lines, x <- 0..(String.length(line) - 1),
          do: {{x, y}, String.at(line, x) |> String.to_integer()}
    end)
    |> Map.new()
    |> count_visible()
    |> IO.puts()
  end

  defp count_visible(grid) do
    grid
    |> Map.keys()
    |> Enum.filter(&is_visible(&1, grid))
    |> length()
  end

  defp is_visible(point, grid) do
    neighbors = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
    Enum.any?(neighbors, fn neighbor ->
      is_visible_in_direction(point, neighbor, grid)
    end)
  end

  defp is_visible_in_direction(point, direction, grid) do
    {x, y} = point
    {dx, dy} = direction
    current_height = grid[point]

    case find_next_tree({x + dx, y + dy}, direction, grid, current_height) do
      nil -> true
      _ -> false
    end
  end

  defp find_next_tree(point, direction, grid, current_height) do
    case grid[point] do
      nil -> nil
      height when height >= current_height -> point
      _ -> find_next_tree(add_points(point, direction), direction, grid, current_height)
    end
  end

  defp add_points({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}
end

Solution.solve()
