
defmodule Solver do
  def solve do
    heightmap =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line
        |> String.to_charlist()
        |> Enum.map(&(&1 - ?0))
      end)

    basin_sizes =
      for y <- 0..(length(heightmap) - 1), x <- 0..(length(Enum.at(heightmap, 0)) - 1), is_low_point(heightmap, x, y) do
        explore_basin(heightmap, x, y, MapSet.new()) |> elem(1)
      end

    basin_sizes
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.reduce(1, &*/2)
    |> IO.puts()
  end

  defp is_low_point(heightmap, x, y) do
    height = Enum.at(Enum.at(heightmap, y), x)

    neighbors =
      for dx <- -1..1,
          dy <- -1..1,
          abs(dx) + abs(dy) == 1,
          new_x = x + dx,
          new_y = y + dy,
          new_x >= 0 and new_x < length(Enum.at(heightmap, 0)),
          new_y >= 0 and new_y < length(heightmap) do
        Enum.at(Enum.at(heightmap, new_y), new_x)
      end

    Enum.all?(neighbors, &(&1 > height))
  end

  defp explore_basin(heightmap, x, y, visited) do
    cond do
      MapSet.member?(visited, {x, y}) or Enum.at(Enum.at(heightmap, y), x) == 9 ->
        {visited, 0}

      true ->
        visited = MapSet.put(visited, {x, y})
        size = 1

        {visited, size} =
          [[0, -1], [-1, 0], [0, 1], [1, 0]]
          |> Enum.reduce({visited, size}, fn [dx, dy], {visited, acc_size} ->
            new_x = x + dx
            new_y = y + dy

            if new_x >= 0 and new_x < length(Enum.at(heightmap, 0)) and new_y >= 0 and new_y < length(heightmap) do
              {visited, new_size} = explore_basin(heightmap, new_x, new_y, visited)
              {visited, acc_size + new_size}
            else
              {visited, acc_size}
            end
          end)

        {visited, size}
    end
  end
end

Solver.solve()
