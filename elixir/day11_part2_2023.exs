
defmodule Solution do
  def solve(input, expansion_factor) do
    grid = build_grid(input)
    expanded_grid = expand_grid(grid, expansion_factor)

    expanded_grid
    |> Map.keys()
    |> Enum.reduce({0, MapSet.new()}, fn coord1, {total, seen} ->
      length = Enum.reduce(seen, 0, fn coord2, acc ->
        acc + calculate_length(coord1, coord2)
      end)
      {total + length, MapSet.put(seen, coord1)}
    end)
    |> elem(0)
  end

  defp build_grid(input) do
    input
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, x}, inner_acc ->
        if char != ".", do: Map.put(inner_acc, {x, y}, char), else: inner_acc
      end)
    end)
  end

  defp expand_grid(grid, expansion_factor) do
    empty_cols = get_empty_cols(grid)
    empty_rows = get_empty_rows(grid)
    num_lines_to_add = expansion_factor - 1

    grid
    |> Map.keys()
    |> Enum.reduce(%{}, fn {x, y}, new_grid ->
      new_x = x + Enum.count(empty_cols, fn col -> col < x end) * num_lines_to_add
      new_y = y + Enum.count(empty_rows, fn row -> row < y end) * num_lines_to_add
      Map.put(new_grid, {new_x, new_y}, grid[{x, y}])
    end)
  end

  defp get_empty_rows(grid) do
    max_y = grid |> Map.keys() |> Enum.map(&elem(&1, 1)) |> Enum.max()
    
    0..max_y
    |> Enum.filter(fn y ->
      not Enum.any?(grid, fn {{_, row_y}, _} -> row_y == y end)
    end)
  end

  defp get_empty_cols(grid) do
    max_x = grid |> Map.keys() |> Enum.map(&elem(&1, 0)) |> Enum.max()
    
    0..max_x
    |> Enum.filter(fn x ->
      not Enum.any?(grid, fn {{col_x, _}, _} -> col_x == x end)
    end)
  end

  defp calculate_length({x1, y1}, {x2, y2}) do
    abs(x2 - x1) + abs(y2 - y1)
  end

  def read_input(filename) do
    filename
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
  end

  def main do
    input = read_input("input.txt")
    IO.puts(solve(input, 1_000_000))
  end
end

Solution.main()
