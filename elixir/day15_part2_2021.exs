
defmodule RiskPath do
  def solve do
    grid = read_input()
    extended_grid = extend_grid(grid)
    dijkstra(extended_grid)
  end

  def read_input do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line |> String.graphemes() |> Enum.map(&String.to_integer/1)
    end)
  end

  def extend_grid(initial_grid) do
    rows = length(initial_grid)
    cols = length(Enum.at(initial_grid, 0))

    for i <- 0..(rows * 5 - 1), j <- 0..(cols * 5 - 1) do
      base_risk = Enum.at(Enum.at(initial_grid, rem(i, rows)), rem(j, cols))
      new_risk = base_risk + div(i, rows) + div(j, cols)
      if new_risk > 9, do: new_risk - 9, else: new_risk
    end
    |> Enum.chunk_every(cols * 5)
  end

  def dijkstra(grid) do
    rows = length(grid)
    cols = length(Enum.at(grid, 0))
    
    initial_state = %{
      queue: [{0, 0, 0}],
      visited: MapSet.new(),
      distances: %{{0, 0} => 0}
    }

    do_dijkstra(grid, rows - 1, cols - 1, initial_state)
  end

  defp do_dijkstra(grid, target_row, target_col, state) do
    case state.queue do
      [] -> 
        Map.get(state.distances, {target_row, target_col})
      
      [{risk, x, y} | rest] ->
        if {x, y} == {target_row, target_col} do
          risk
        else
          new_state = 
            if {x, y} in state.visited do
              %{state | queue: rest}
            else
              neighbors = [
                {x+1, y}, {x-1, y}, 
                {x, y+1}, {x, y-1}
              ]
              
              updated_state = 
                Enum.reduce(neighbors, %{state | visited: MapSet.put(state.visited, {x, y}), queue: rest}, 
                  fn {nx, ny}, acc ->
                    if nx >= 0 and ny >= 0 and nx < length(grid) and ny < length(Enum.at(grid, 0)) do
                      neighbor_risk = Enum.at(Enum.at(grid, nx), ny)
                      total_risk = risk + neighbor_risk
                      
                      if total_risk < Map.get(acc.distances, {nx, ny}, :infinity) do
                        updated_distances = Map.put(acc.distances, {nx, ny}, total_risk)
                        updated_queue = 
                          [{total_risk, nx, ny} | acc.queue]
                          |> Enum.sort()
                        
                        %{acc | distances: updated_distances, queue: updated_queue}
                      else
                        acc
                      end
                    else
                      acc
                    end
                  end)
              
              updated_state
            end
          
          do_dijkstra(grid, target_row, target_col, new_state)
        end
    end
  end
end

RiskPath.solve() |> IO.puts()
