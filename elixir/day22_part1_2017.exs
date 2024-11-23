
defmodule Virus do
  def solve do
    grid = 
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {char, x}, inner_acc ->
          if char == "#", do: Map.put(inner_acc, {x, y}, true), else: inner_acc
        end)
      end)

    start_x = grid |> Map.keys() |> Enum.map(fn {x, _} -> x end) |> Enum.max() |> div(2)
    start_y = grid |> Map.keys() |> Enum.map(fn {_, y} -> y end) |> Enum.max() |> div(2)

    directions = [{0, -1}, {1, 0}, {0, 1}, {-1, 0}]

    result = 
      Enum.reduce(1..10000, {grid, {start_x, start_y}, 0, 0}, fn _, {grid, {x, y}, dir, infected_count} ->
        case Map.get(grid, {x, y}, false) do
          true -> 
            # Infected - turn right, clean
            new_dir = rem(dir + 1, 4)
            new_grid = Map.delete(grid, {x, y})
            {new_dx, new_dy} = Enum.at(directions, new_dir)
            {new_grid, {x + new_dx, y + new_dy}, new_dir, infected_count}
          
          false -> 
            # Clean - turn left, infect
            new_dir = rem(dir - 1 + 4, 4)
            new_grid = Map.put(grid, {x, y}, true)
            {new_dx, new_dy} = Enum.at(directions, new_dir)
            {new_grid, {x + new_dx, y + new_dy}, new_dir, infected_count + 1}
        end
      end)

    {_, _, _, infected_count} = result
    IO.puts(infected_count)
  end
end

Virus.solve()
