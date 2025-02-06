
defmodule Day25 do
  def solve do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
    |> find_safe_step(0)
  end

  defp find_safe_step(grid, step) do
    {east_moved_grid, east_moved} = move_east(grid)
    {south_moved_grid, south_moved} = move_south(east_moved_grid)

    if east_moved || south_moved do
      find_safe_step(south_moved_grid, step + 1)
    else
      step + 1
    end
  end

  defp move_east(grid) do
    do_move(grid, ?>, {0, 1})
  end

  defp move_south(grid) do
    do_move(grid, ?v, {1, 0})
  end

  defp do_move(grid, cucumber, {dy, dx}) do
    height = length(grid)
    width = length(hd(grid))
    moved =
      for y <- 0..(height - 1), x <- 0..(width - 1), grid_get(grid, y, x) == cucumber do
        next_y = rem(y + dy, height)
        next_x = rem(x + dx, width)

        if grid_get(grid, next_y, next_x) == ?. do
          {{y, x}, {next_y, next_x}}
        end
      end
      |> Enum.filter(& &1)
    new_grid = update_grid(grid,moved)
    {new_grid, length(moved) > 0}
  end

    defp update_grid(grid, moved) do
    moved_map =
        moved
        |> Enum.reduce(%{}, fn {from, to}, acc ->
          acc |> Map.put(from, :empty) |> Map.put(to, :moved)
          end)
    
        Enum.map(grid, fn row ->
          Enum.map(row, fn _ ->
            
          end)
        end)

    height = length(grid)
    width = length(hd(grid))
      
      for y <- 0..(height - 1), into: [] do
        for x <- 0..(width - 1), into: [] do
          case moved_map[{y,x}] do
            :empty -> ?.
            :moved ->
              case Enum.find(moved, fn {_, to} -> to == {y, x} end) do
                {{prev_y, prev_x}, _} -> grid_get(grid, prev_y, prev_x)
                nil -> grid_get(grid,y,x)
              end
            nil -> grid_get(grid, y, x)
          end
        end
      end
  end

  defp grid_get(grid, y, x) do
    Enum.at(grid, y) |> Enum.at(x)
  end
end

IO.puts(Day25.solve)
