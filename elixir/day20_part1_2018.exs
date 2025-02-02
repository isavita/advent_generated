
defmodule Day20 do
  def read_input(filename) do
    File.read!(filename) |> String.trim()
  end

  def parse(regex) do
    {grid, _} = parse(regex, 1, %{}, {0, 0}, [{0, 0}])
    grid
  end

  defp parse(regex, index, grid, pos, stack) do
    case String.at(regex, index) do
      "^" ->
        parse(regex, index + 1, grid, pos, stack)

      "$" ->
        {grid, index}

      "N" ->
        {x, y} = pos
        new_pos = {x, y - 1}
        grid = Map.put(grid, {pos, new_pos}, 1)
        grid = Map.put(grid, {new_pos, pos}, 1)
        parse(regex, index + 1, grid, new_pos, stack)

      "S" ->
        {x, y} = pos
        new_pos = {x, y + 1}
        grid = Map.put(grid, {pos, new_pos}, 1)
        grid = Map.put(grid, {new_pos, pos}, 1)
        parse(regex, index + 1, grid, new_pos, stack)

      "E" ->
        {x, y} = pos
        new_pos = {x + 1, y}
        grid = Map.put(grid, {pos, new_pos}, 1)
        grid = Map.put(grid, {new_pos, pos}, 1)
        parse(regex, index + 1, grid, new_pos, stack)

      "W" ->
        {x, y} = pos
        new_pos = {x - 1, y}
        grid = Map.put(grid, {pos, new_pos}, 1)
        grid = Map.put(grid, {new_pos, pos}, 1)
        parse(regex, index + 1, grid, new_pos, stack)

      "(" ->
        parse(regex, index + 1, grid, pos, [pos | stack])

      "|" ->
        {grid, new_index} = parse(regex, index + 1, grid, hd(stack), stack)
        {grid, new_index}

      ")" ->
        {grid, new_index} = parse(regex, index + 1, grid, pos, tl(stack))
        {grid, new_index}
      nil -> {grid, index}

      _ ->
        {grid, index}
    end
  end

  def bfs(grid, start) do
    queue = [{start, 0}]
    visited = MapSet.new([start])
    distances = %{}

    bfs_helper(grid, queue, visited, distances)
  end

  defp bfs_helper(grid, [], _, distances) do
    distances
  end

  defp bfs_helper(grid, [{current, dist} | rest], visited, distances) do
    distances = Map.put(distances, current, dist)

    neighbors =
      grid
      |> Map.keys()
      |> Enum.filter(fn {a, _} -> a == current end)
      |> Enum.map(fn {_, b} -> b end)
      |> Enum.reject(&MapSet.member?(visited, &1))

    new_visited = Enum.reduce(neighbors, visited, &MapSet.put(&2, &1))
    new_queue = rest ++ Enum.map(neighbors, &{&1, dist + 1})

    bfs_helper(grid, new_queue, new_visited, distances)
  end

  def solve(filename) do
    regex = read_input(filename)
    grid = parse(regex)
    distances = bfs(grid, {0, 0})
    Enum.max_by(distances, fn {_, v} -> v end) |> elem(1)
  end
end

result = Day20.solve("input.txt")
IO.puts(result)
