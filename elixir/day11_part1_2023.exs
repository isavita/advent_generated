
defmodule Grid do
  defstruct [:width, :height, :data]

  def build(input, empty) do
    data =
      input
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, y} ->
        line
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.filter(fn {char, _} -> char != empty end)
        |> Enum.map(fn {char, x} -> {{x, y}, char} end)
      end)
      |> Map.new()

    %Grid{
      width: String.length(hd(input)),
      height: length(input),
      data: data
    }
  end

  def get_empty_rows(grid) do
    0..(grid.height - 1)
    |> Enum.filter(fn y ->
      0..(grid.width - 1)
      |> Enum.all?(fn x -> not Map.has_key?(grid.data, {x, y}) end)
    end)
  end

  def get_empty_cols(grid) do
    0..(grid.width - 1)
    |> Enum.filter(fn x ->
      0..(grid.height - 1)
      |> Enum.all?(fn y -> not Map.has_key?(grid.data, {x, y}) end)
    end)
  end

  def calculate_offsets(empty_indexes, bound) do
    0..(bound - 1)
    |> Enum.map(fn i ->
      Enum.count(empty_indexes, &(&1 < i))
    end)
  end

  def expand(grid, expansion_factor) do
    empty_cols = get_empty_cols(grid)
    empty_rows = get_empty_rows(grid)
    num_lines_to_add = expansion_factor - 1

    dXs = calculate_offsets(empty_cols, grid.width)
    dYs = calculate_offsets(empty_rows, grid.height)

    new_data =
      grid.data
      |> Enum.map(fn {{x, y}, char} ->
        new_x = x + Enum.at(dXs, x) * num_lines_to_add
        new_y = y + Enum.at(dYs, y) * num_lines_to_add
        {{new_x, new_y}, char}
      end)
      |> Map.new()

    %Grid{
      width: grid.width + length(empty_cols) * num_lines_to_add,
      height: grid.height + length(empty_rows) * num_lines_to_add,
      data: new_data
    }
  end

  def calculate_length({x1, y1}, {x2, y2}) do
    abs(x2 - x1) + abs(y2 - y1)
  end

  def solve(input) do
    grid = build(input, ?.)
    expanded_grid = expand(grid, 2)

    expanded_grid.data
    |> Enum.reduce({0, MapSet.new()}, fn {coord1, _}, {acc, seen} ->
      new_acc =
        seen
        |> Enum.reduce(acc, fn coord2, acc ->
          acc + calculate_length(coord1, coord2)
        end)

      {new_acc, MapSet.put(seen, coord1)}
    end)
    |> elem(0)
  end
end

input = File.read!("input.txt") |> String.trim() |> String.split("\n")
IO.puts(Grid.solve(input))
