
defmodule Day10 do
  @empty "."
  @start "S"
  @vertical "|"
  @horizontal "-"
  @top_left_corner "J"
  @top_right_corner "L"
  @bottom_left_corner "7"
  @bottom_right_corner "F"
  @enclosed "X"

  @tile_to_pipe %{
    @vertical => %{top: true, bottom: true},
    @horizontal => %{left: true, right: true},
    @top_left_corner => %{top: true, left: true},
    @top_right_corner => %{top: true, right: true},
    @bottom_left_corner => %{bottom: true, left: true},
    @bottom_right_corner => %{bottom: true, right: true}
  }

  def solve(input) do
    grid = build_grid(input)
    start = find_start(grid)
    path = path_finding(start, grid)
    length(path) / 2
  end

  def build_grid(input) do
    input
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _x} -> char != @empty end)
      |> Enum.map(fn {char, x} -> {{x, y}, char} end)
    end)
    |> Map.new()
  end

  def find_start(grid) do
    grid
    |> Enum.find(fn {{_x, _y}, char} -> char == @start end)
    |> case do
      {{x, y}, _char} -> {x, y}
    end
  end

  def get_pipe_from_tile(tile) do
    Map.get(@tile_to_pipe, tile, %{})
  end

  def get_tile_from_pipe(pipe) do
    @tile_to_pipe
    |> Enum.find(fn {_tile, associated_pipe} -> pipe == associated_pipe end)
    |> case do
      {tile, _pipe} -> tile
      nil -> @empty
    end
  end

  def get_pipe_from_neighbors({x, y}, grid) do
    possible_neighbors = %{
      top: {x, y - 1},
      right: {x + 1, y},
      bottom: {x, y + 1},
      left: {x - 1, y}
    }

    possible_neighbors
    |> Enum.reduce(%{}, fn {dir, {nx, ny}}, acc ->
      case Map.get(grid, {nx, ny}) do
        nil ->
          acc

        tile ->
          neighbor_pipe = get_pipe_from_tile(tile)

          if Map.has_key?(neighbor_pipe, opposite_dir(dir)) do
            Map.put(acc, dir, true)
          else
            acc
          end
      end
    end)
  end

  def opposite_dir(dir) do
    case dir do
      :top -> :bottom
      :right -> :left
      :bottom -> :top
      :left -> :right
    end
  end

  def path_finding(start, grid) do
    start_pipe = get_pipe_from_neighbors(start, grid)
    {previous_dir, _} = Enum.find(start_pipe, fn {_, _} -> true end)
    current = move(start, previous_dir)
    path = [start, current]

    path_finding(current, previous_dir, start, path, grid)
  end

  def path_finding(current, previous_dir, start, path, grid) do
    if current == start do
      path
    else
      current_pipe = get_pipe_from_tile(Map.get(grid, current))

      {new_dir, _} =
        current_pipe
        |> Enum.find(fn {dir, _} -> dir != opposite_dir(previous_dir) end)

      next_coord = move(current, new_dir)
      path_finding(next_coord, new_dir, start, path ++ [next_coord], grid)
    end
  end

  def move({x, y}, dir) do
    case dir do
      :top -> {x, y - 1}
      :right -> {x + 1, y}
      :bottom -> {x, y + 1}
      :left -> {x - 1, y}
    end
  end

  def read_file(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
  end
end

input = Day10.read_file("input.txt")
IO.puts(Day10.solve(input))
