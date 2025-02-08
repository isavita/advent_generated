
defmodule Day18 do
  def read_input(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&parse_line/1)
    |> Enum.to_list()
  end

  defp parse_line(line) do
    line
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  def solve(input) do
    {part1, _} = solve1(input, 1024)
    part2 = solve2(input)
    {part1, part2}
  end

  def solve1(input, limit) do
    grid = %{}
    {min_steps, final_grid} = run_simulation(input, grid, 0, limit, {0, 0}, {70, 70})
    {min_steps, final_grid}
  end
    
  def solve2(input) do
      grid = %{}

      Enum.reduce_while(Enum.with_index(input), grid, fn {coords, idx}, acc_grid ->
          new_grid = Map.put(acc_grid, coords, true)
          case path_exists?(new_grid, {0, 0}, {70, 70}) do
              true ->
                  {:cont, new_grid}
              false ->
                {x,y} = coords
                  {:halt, "#{x},#{y}"}
          end
      end)
  end
    
  def run_simulation(input, grid, steps_taken, limit, start, finish) do
      input
      |> Enum.take(limit)
      |> Enum.reduce({grid, steps_taken}, fn coord, {current_grid, _} ->
           new_grid = Map.put(current_grid, coord, true)
          {new_grid, 0}
       end)
      |> case do
        {final_grid, _} -> {bfs(final_grid, start, finish), final_grid}
      end

  end

  def bfs(grid, start, finish) do
    queue = [start]
    visited = MapSet.new([start])
    distances = %{start => 0}

    bfs_helper(queue, visited, distances, finish, grid)
  end

  defp bfs_helper([], _, _, _, _) do
    :infinity
  end

  defp bfs_helper([current | rest], visited, distances, finish, grid) do
    if current == finish do
      distances[current]
    else
      neighbors = get_neighbors(current, grid)
      unvisited_neighbors =
        Enum.filter(neighbors, fn neighbor ->
          not MapSet.member?(visited, neighbor) and not Map.get(grid, neighbor, false)
        end)

      new_visited = Enum.reduce(unvisited_neighbors, visited, &MapSet.put(&2, &1))

      new_distances =
        Enum.reduce(unvisited_neighbors, distances, fn neighbor, acc ->
          Map.put(acc, neighbor, distances[current] + 1)
        end)

      bfs_helper(rest ++ unvisited_neighbors, new_visited, new_distances, finish, grid)
    end
  end

  def get_neighbors({x, y}, _grid) do
      [
        {x - 1, y},
        {x + 1, y},
        {x, y - 1},
        {x, y + 1}
      ]
      |> Enum.filter(fn {nx, ny} -> nx >= 0 and nx <= 70 and ny >= 0 and ny <= 70 end)
  end

    
  def path_exists?(grid, start, finish) do
    bfs(grid, start, finish) != :infinity
  end
end

input = Day18.read_input("input.txt")
{part1, part2} = Day18.solve(input)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
