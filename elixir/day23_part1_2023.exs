
defmodule Day23 do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {row, y}, acc ->
      row
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, x}, acc2 ->
        if char != ?# do
          Map.put(acc2, {x, y}, char)
        else
          acc2
        end
      end)
    end)
  end

  def solve(filename) do
    grid = read_input(filename)
    start = Enum.find(grid, fn {{x, y}, _} -> y == 0 end) |> elem(0)
    finish_y = grid |> Enum.map(fn {{_, y}, _} -> y end) |> Enum.max()
    finish = Enum.find(grid, fn {{x, y}, _} -> y == finish_y end) |> elem(0)

    find_longest_path(grid, start, finish, MapSet.new())
  end

  defp find_longest_path(grid, current, finish, visited) do
    if current == finish do
      MapSet.size(visited)
    else
      visited = MapSet.put(visited, current)

      neighbors =
        case grid[current] do
          ?. ->
            [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
            |> Enum.map(fn {dx, dy} -> {elem(current, 0) + dx, elem(current, 1) + dy} end)
            |> Enum.filter(fn next_pos ->
              grid[next_pos] && !MapSet.member?(visited, next_pos)
            end)

          ?^ -> [ {0, -1} ]
                |> Enum.map(fn {dx, dy} -> {elem(current, 0) + dx, elem(current, 1) + dy} end)
                |> Enum.filter(fn next_pos -> grid[next_pos] && !MapSet.member?(visited,next_pos) end)
          ?> -> [ {1, 0} ]
                |> Enum.map(fn {dx, dy} -> {elem(current, 0) + dx, elem(current, 1) + dy} end)
                |> Enum.filter(fn next_pos -> grid[next_pos] && !MapSet.member?(visited,next_pos) end)

          ?v -> [ {0, 1} ]
              |> Enum.map(fn {dx, dy} -> {elem(current, 0) + dx, elem(current, 1) + dy} end)
              |> Enum.filter(fn next_pos -> grid[next_pos] && !MapSet.member?(visited,next_pos) end)
          ?< -> [ {-1, 0} ]
              |> Enum.map(fn {dx, dy} -> {elem(current, 0) + dx, elem(current, 1) + dy} end)
              |> Enum.filter(fn next_pos -> grid[next_pos] && !MapSet.member?(visited,next_pos) end)

        end


      if Enum.empty?(neighbors) do
        0
      else
        neighbors
        |> Enum.map(fn neighbor -> find_longest_path(grid, neighbor, finish, visited) end)
        |> Enum.max()
      end
    end
  end


  def solve2(filename) do
      grid = read_input(filename)
      start = Enum.find(grid, fn {{x, y}, _} -> y == 0 end) |> elem(0)
      finish_y = grid |> Enum.map(fn {{_, y}, _} -> y end) |> Enum.max()
      finish = Enum.find(grid, fn {{x, y}, _} -> y == finish_y end) |> elem(0)

      find_longest_path2(grid, start, finish, MapSet.new())
    end

  defp find_longest_path2(grid, current, finish, visited) do
      if current == finish do
        MapSet.size(visited)
      else
        visited = MapSet.put(visited, current)
        neighbors =  [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
                      |> Enum.map(fn {dx, dy} -> {elem(current, 0) + dx, elem(current, 1) + dy} end)
                      |> Enum.filter(fn next_pos ->
                        grid[next_pos] && !MapSet.member?(visited, next_pos)
                      end)

        if Enum.empty?(neighbors) do
          0
        else
          neighbors
          |> Enum.map(fn neighbor -> find_longest_path2(grid, neighbor, finish, visited) end)
          |> Enum.max()
        end
      end
  end
end

Day23.solve("input.txt") |> IO.puts()
# Day23.solve2("input.txt") |> IO.puts() # For part 2 (ignoring slopes)
