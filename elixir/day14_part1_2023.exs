defmodule Rocks do
  defstruct width: 0, height: 0, data: %{}

  def run do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
    |> solve()
    |> IO.puts()
  end

  defp solve(input) do
    grid = build_grid(input)
    grid = shift_rocks(grid, {0, -1})
    calculate_load(grid)
  end

  defp build_grid(input) do
    height = length(input)
    width = input |> hd() |> String.length()

    data =
      input
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {char, x}, acc ->
          if char != "." do
            Map.put(acc, {x, y}, char)
          else
            acc
          end
        end)
      end)

    %Rocks{width: width, height: height, data: data}
  end

  defp shift_rocks(grid, {dx, dy}) do
    directions =
      cond do
        dx == 0 and dy == -1 -> for x <- 0..(grid.width - 1), y <- 0..(grid.height - 1), do: {x, y}
        dx == 0 and dy == 1 -> for x <- 0..(grid.width - 1), y <- (grid.height - 1)..0, -1, do: {x, y}
        dx == -1 and dy == 0 -> for x <- 0..(grid.width - 1), y <- 0..(grid.height - 1), do: {x, y}
        dx == 1 and dy == 0 -> for x <- (grid.width - 1)..0, -1, y <- 0..(grid.height - 1), do: {x, y}
      end

    Enum.reduce(directions, grid, fn {x, y}, grid ->
      shift_single_rock(grid, {x, y}, {dx, dy})
    end)
  end

  defp shift_single_rock(grid, {x, y}, {dx, dy}) do
    case Map.fetch(grid.data, {x, y}) do
      {:ok, "O"} ->
        shift_rock(grid, {x, y}, {dx, dy})
      _ ->
        grid
    end
  end

  defp shift_rock(grid, {x, y}, {dx, dy}) do
    new_coord = {x + dx, y + dy}

    if in_bounds?(new_coord, grid.width, grid.height) and Map.get(grid.data, new_coord) == nil do
      data = Map.delete(grid.data, {x, y})
      data = Map.put(data, new_coord, "O")
      %Rocks{grid | data: data}
      |> shift_rock(new_coord, {dx, dy})
    else
      grid
    end
  end

  defp in_bounds?({x, y}, width, height) do
    x >= 0 and x < width and y >= 0 and y < height
  end

  defp calculate_load(grid) do
    Enum.reduce(grid.data, 0, fn
      {{_x, y}, "O"}, acc -> acc + (grid.height - y)
      (_, acc) -> acc
    end)
  end
end

Rocks.run()