
defmodule RiskPath do
  def solve do
    grid = 
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(fn line -> 
        line |> String.graphemes() |> Enum.map(&String.to_integer/1)
      end)

    dijkstra(grid)
  end

  def dijkstra(grid) do
    rows = length(grid)
    cols = length(Enum.at(grid, 0))
    
    initial_state = %{
      queue: [{0, 0, 0}],
      visited: MapSet.new()
    }

    do_dijkstra(grid, initial_state, rows, cols)
  end

  defp do_dijkstra(grid, state, rows, cols) do
    case state.queue do
      [] -> 
        -1

      [{risk, x, y} | rest] when x == rows - 1 and y == cols - 1 -> 
        risk

      [{risk, x, y} | rest] ->
        if MapSet.member?(state.visited, {x, y}) do
          do_dijkstra(grid, %{state | queue: rest}, rows, cols)
        else
          new_visited = MapSet.put(state.visited, {x, y})
          
          new_queue = 
            [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]
            |> Enum.reduce(rest, fn {dx, dy}, acc ->
              nx = x + dx
              ny = y + dy
              
              if nx >= 0 and ny >= 0 and nx < rows and ny < cols and not MapSet.member?(new_visited, {nx, ny}) do
                new_risk = risk + Enum.at(Enum.at(grid, nx), ny)
                insert_sorted(acc, {new_risk, nx, ny})
              else
                acc
              end
            end)
          
          do_dijkstra(grid, %{state | queue: new_queue, visited: new_visited}, rows, cols)
        end
    end
  end

  defp insert_sorted(queue, item) do
    do_insert_sorted(queue, item, [])
  end

  defp do_insert_sorted([], item, acc), do: Enum.reverse([item | acc])
  defp do_insert_sorted([h | t], item, acc) do
    if elem(item, 0) <= elem(h, 0) do
      Enum.reverse(acc) ++ [item | [h | t]]
    else
      do_insert_sorted(t, item, [h | acc])
    end
  end
end

RiskPath.solve() |> IO.puts()
