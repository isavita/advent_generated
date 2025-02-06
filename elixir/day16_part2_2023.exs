
defmodule Day16 do
  def read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
  end

  def energize(grid, start_pos, start_dir) do
    {rows, cols} = {length(grid), length(hd(grid))}
    visited = MapSet.new()
    process_beam(grid, start_pos, start_dir, visited, rows, cols)
    |> MapSet.to_list()
    |> Enum.map(fn {{row, col}, _dir} -> {row, col} end)
    |> MapSet.new()
    |> MapSet.size()
  end

  defp process_beam(grid, {row, col}, dir, visited, rows, cols) do
    if row < 0 or row >= rows or col < 0 or col >= cols or MapSet.member?(visited, {{row, col}, dir}) do
      visited
    else
      visited = MapSet.put(visited, {{row, col}, dir})
      tile = Enum.at(Enum.at(grid, row), col)
      next_dirs = next_directions(tile, dir)

      Enum.reduce(next_dirs, visited, fn next_dir, acc_visited ->
        {next_row, next_col} = move({row, col}, next_dir)
        process_beam(grid, {next_row, next_col}, next_dir, acc_visited, rows, cols)
      end)
    end
  end

  defp next_directions(tile, dir) do
    case {tile, dir} do
      {?., _} -> [dir]
      {?/, {0, 1}} -> [{-1, 0}]
      {?/, {0, -1}} -> [{1, 0}]
      {?/, {1, 0}} -> [{0, -1}]
      {?/, {-1, 0}} -> [{0, 1}]
      {?\\, {0, 1}} -> [{1, 0}]
      {?\\, {0, -1}} -> [{-1, 0}]
      {?\\, {1, 0}} -> [{0, 1}]
      {?\\, {-1, 0}} -> [{0, -1}]
      {?-, {1, 0}} -> [{0, -1}, {0, 1}]
      {?-, {-1, 0}} -> [{0, -1}, {0, 1}]
      {?-, _} -> [dir]
      {?|, {0, 1}} -> [{-1, 0}, {1, 0}]
      {?|, {0, -1}} -> [{-1, 0}, {1, 0}]
      {?|, _} -> [dir]
    end
  end

  defp move({row, col}, {dr, dc}) do
    {row + dr, col + dc}
  end

  def part1(filename) do
    grid = read_input(filename)
    energize(grid, {0, 0}, {0, 1})
  end

  def part2(filename) do
    grid = read_input(filename)
    {rows, cols} = {length(grid), length(hd(grid))}

    top_starts = for col <- 0..(cols - 1), do: {{0, col}, {1, 0}}
    bottom_starts = for col <- 0..(cols - 1), do: {{rows - 1, col}, {-1, 0}}
    left_starts = for row <- 0..(rows - 1), do: {{row, 0}, {0, 1}}
    right_starts = for row <- 0..(rows - 1), do: {{row, cols - 1}, {0, -1}}

    all_starts = top_starts ++ bottom_starts ++ left_starts ++ right_starts

    Enum.map(all_starts, fn {start_pos, start_dir} ->
      energize(grid, start_pos, start_dir)
    end)
    |> Enum.max()
  end
end

input_file = "input.txt"
result1 = Day16.part1(input_file)
IO.puts("Part 1: #{result1}")

result2 = Day16.part2(input_file)
IO.puts("Part 2: #{result2}")
