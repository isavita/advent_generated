
defmodule HillClimbing do
  def read_input(file) do
    file
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  def find_positions(grid, char) do
    for {row, y} <- Enum.with_index(grid),
        {value, x} <- Enum.with_index(row),
        value == char,
        do: {x, y}
  end

  def elevation(char) when char in ?a..?z, do: char - ?a
  def elevation(?S), do: elevation(?a)
  def elevation(?E), do: elevation(?z)

  def neighbors({x, y}, grid) do
    [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
    |> Enum.filter(&valid_move?(&1, {x, y}, grid))
  end

  def valid_move?({nx, ny}, {x, y}, grid) do
    nx >= 0 and ny >= 0 and nx < length(Enum.at(grid, 0)) and ny < length(grid) and
      elevation(Enum.at(Enum.at(grid, ny), nx)) - elevation(Enum.at(Enum.at(grid, y), x)) <= 1
  end

  def bfs(start, goal, grid) do
    queue = :queue.from_list([{start, 0}])
    visited = MapSet.new([start])

    bfs_loop(queue, visited, goal, grid)
  end

  defp bfs_loop(queue, visited, goal, grid) do
    case :queue.out(queue) do
      {{:value, {current, steps}}, queue} ->
        if current == goal do
          steps
        else
          new_positions =
            neighbors(current, grid)
            |> Enum.filter(&(!MapSet.member?(visited, &1)))

          new_queue = Enum.reduce(new_positions, queue, fn pos, q -> :queue.in({pos, steps + 1}, q) end)
          new_visited = Enum.reduce(new_positions, visited, &MapSet.put(&2, &1))

          bfs_loop(new_queue, new_visited, goal, grid)
        end

      {:empty, _} ->
        :infinity
    end
  end

  def solve(file) do
    grid = read_input(file)
    [start] = find_positions(grid, ?S)
    [goal] = find_positions(grid, ?E)

    bfs(start, goal, grid)
  end
end

# Run the solution
IO.puts(HillClimbing.solve("input.txt"))
