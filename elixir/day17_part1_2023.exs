
defmodule Day17 do
  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn row ->
      row
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def solve(grid) do
    rows = length(grid)
    cols = length(Enum.at(grid, 0))
    start = {0, 0}
    goal = {rows - 1, cols - 1}

    # Priority queue: {heat_loss, row, col, dir_row, dir_col, steps}
    queue = :gb_trees.enter({0, start, {0, 0}, 0}, 0, :gb_trees.empty())
    visited = MapSet.new()

    directions = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]  # Right, Down, Left, Up

    find_path(queue, visited, grid, goal, directions)
  end
    
  defp find_path(queue, visited, grid, goal, directions) do
      case :gb_trees.take_smallest(queue) do
          {{heat_loss, {row, col} = current, {dr, dc}, steps}, _, new_queue} ->
              
            if current == goal do
              heat_loss
            else
              if MapSet.member?(visited, {current, {dr, dc}, steps}) do
                find_path(new_queue, visited, grid, goal, directions)
              else
                  
                visited = MapSet.put(visited, {current, {dr, dc}, steps})

                new_queue =
                  directions
                  |> Enum.reduce(new_queue, fn {ndr, ndc}, acc_queue ->
                    nr = row + ndr
                    nc = col + ndc

                    if valid_move?(grid, nr, nc) do
                      new_steps = if {ndr, ndc} == {dr, dc}, do: steps + 1, else: 1
                      if new_steps <= 3 and ({dr, dc} == {0, 0} or {ndr, ndc} != {-dr, -dc}) do # Initial move, or not reversing
                        new_heat_loss = heat_loss + Enum.at(Enum.at(grid, nr), nc)
                        :gb_trees.enter({new_heat_loss, {nr, nc}, {ndr, ndc}, new_steps}, 0, acc_queue)
                      else
                        acc_queue
                      end
                    else
                      acc_queue
                    end
                  end)

                find_path(new_queue, visited, grid, goal, directions)
              end
          end
          
          :empty ->
            nil # Should never happen with valid input if a path is possible
      end
    end

  defp valid_move?(grid, row, col) do
    rows = length(grid)
    cols = length(Enum.at(grid, 0))
    row >= 0 and row < rows and col >= 0 and col < cols
  end
end

grid = Day17.read_input("input.txt")
result = Day17.solve(grid)
IO.puts(result)
