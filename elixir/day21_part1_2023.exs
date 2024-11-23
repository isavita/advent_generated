
defmodule Garden do
  def solve(input, num_steps) do
    grid = parse_input(input)
    start = find_start(grid)
    distances = breadth_first_search(grid, start)

    Enum.count(distances, fn {_, dist} -> 
      dist <= num_steps and rem(dist, 2) == 0 
    end)
  end

  def parse_input(input) do
    input
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, x}, inner_acc ->
        case char do
          "#" -> Map.put(inner_acc, {x, y}, :rock)
          "S" -> Map.put(inner_acc, {x, y}, :start)
          _ -> inner_acc
        end
      end)
    end)
    |> then(&%{data: &1, width: String.length(Enum.at(input, 0)), height: length(input)})
  end

  def find_start(grid) do
    grid.data
    |> Enum.find(fn {_, val} -> val == :start end)
    |> elem(0)
  end

  def breadth_first_search(grid, start) do
    initial_state = %{
      frontier: :queue.from_list([start]),
      reached: MapSet.new([start]),
      distances: %{start => 0}
    }

    do_bfs(grid, initial_state)
  end

  defp do_bfs(grid, state) do
    case :queue.out(state.frontier) do
      {{:value, current}, new_frontier} ->
        neighbors = get_neighbors(grid, current)
        
        {updated_frontier, updated_reached, updated_distances} = 
          Enum.reduce(neighbors, {new_frontier, state.reached, state.distances}, 
            fn next, {f, r, d} ->
              if not MapSet.member?(r, next) do
                {
                  :queue.in(next, f), 
                  MapSet.put(r, next), 
                  Map.put(d, next, d[current] + 1)
                }
              else
                {f, r, d}
              end
            end)

        do_bfs(grid, %{
          frontier: updated_frontier, 
          reached: updated_reached, 
          distances: updated_distances
        })

      {:empty, _} ->
        state.distances
    end
  end

  defp get_neighbors(grid, {x, y}) do
    [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
    |> Enum.map(fn {dx, dy} -> {x + dx, y + dy} end)
    |> Enum.filter(fn coord -> 
      in_bounds?(grid, coord) and not is_rock?(grid, coord) 
    end)
  end

  defp in_bounds?(grid, {x, y}) do
    x >= 0 and x < grid.width and y >= 0 and y < grid.height
  end

  defp is_rock?(grid, coord) do
    Map.get(grid.data, coord) == :rock
  end

  def read_input(filename) do
    filename
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
  end
end

input = Garden.read_input("input.txt")
result = Garden.solve(input, 64)
IO.puts(result)
