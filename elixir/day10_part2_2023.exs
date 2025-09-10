defmodule Pipe do
  defstruct coords: []
end

defmodule Grid do
  defstruct width: 0, height: 0, data: %{}
end

defmodule Solver do
  @top {0, -1}
  @right {1, 0}
  @bottom {0, 1}
  @left {-1, 0}

  @empty "."
  @start "S"
  @vertical "|"
  @horizontal "-"
  @tl "J"
  @tr "L"
  @bl "7"
  @br "F"

  @top_token @top
  @bottom_token @bottom
  @left_token @left
  @right_token @right

  def add({x1, y1}, {dx, dy}), do: {x1 + dx, y1 + dy}
  def opposite({dx, dy}), do: {-dx, -dy}

  def pipe_from_tile(nil), do: %Pipe{coords: []}
  def pipe_from_tile(ch) do
    case ch do
      @vertical -> %Pipe{coords: [@top, @bottom]}
      @horizontal -> %Pipe{coords: [@left, @right]}
      @tl -> %Pipe{coords: [@top, @left]}
      @tr -> %Pipe{coords: [@top, @right]}
      @bl -> %Pipe{coords: [@bottom, @left]}
      @br -> %Pipe{coords: [@bottom, @right]}
      _ -> %Pipe{coords: []}
    end
  end

  def tile_from_pipe(%Pipe{coords: coords}) do
    case coords do
      [@top, @bottom] -> @vertical
      [@left, @right] -> @horizontal
      [@top, @left] -> @tl
      [@top, @right] -> @tr
      [@bottom, @left] -> @bl
      [@bottom, @right] -> @br
      _ -> @empty
    end
  end

  def get_pipe_from_tile(ch) do
    pipe_from_tile(ch)
  end

  def get_pipe_from_neighbors(coord, %Grid{data: data}) do
    dirs = [@top, @right, @bottom, @left]
    pipes = Enum.reduce(dirs, [], fn dir, acc ->
      neighbor = add(coord, dir)
      neighbor_tile = Map.get(data, neighbor)
      neighbor_pipe = pipe_from_tile(neighbor_tile)
      if Enum.member?(neighbor_pipe.coords, opposite(dir)) do
        acc ++ [dir]
      else
        acc
      end
    end)
    %Pipe{coords: pipes}
  end

  def build_grid(lines) do
    width = lines |> List.first() |> String.length()
    height = length(lines)
    data = Enum.reduce(0..(height - 1), %{}, fn y, acc ->
      line = Enum.at(lines, y)
      chars = String.graphemes(line)
      Enum.reduce(0..(width - 1), acc, fn x, acc2 ->
        ch = Enum.at(chars, x)
        if ch != @empty do
          Map.put(acc2, {x, y}, ch)
        else
          acc2
        end
      end)
    end)
    %Grid{width: width, height: height, data: data}
  end

  def find_start(%Grid{data: data}) do
    case Enum.find(data, fn {_coord, val} -> val == @start end) do
      {coord, _} -> coord
      nil -> nil
    end
  end

  def path_finding(start, grid) do
    start_pipe = get_pipe_from_neighbors(start, grid).coords

    {path, current, prev_dir} =
      Enum.reduce(start_pipe, { [start], nil, nil }, fn dir, acc ->
        {path, _curr, _} = acc
        {path, add(start, dir), dir}
      end)

    do_path_finding(path, current, prev_dir, grid)
  end

  def do_path_finding(path, current, prev_dir, grid) do
    first = List.first(path)
    if current == first do
      path
    else
      current_tile = Map.get(grid.data, current)
      current_pipe = pipe_from_tile(current_tile).coords
      next_dir = Enum.find(current_pipe, fn d -> d != opposite(prev_dir) end)
      do_path_finding(path ++ [current], add(current, next_dir), next_dir, grid)
    end
  end

  def get_path_grid(grid, path, _empty) do
    new_data = Enum.reduce(path, %{}, fn coord, acc ->
      Map.put(acc, coord, Map.get(grid.data, coord))
    end)

    start = List.first(path)
    path_start_pipe = get_pipe_from_neighbors(start, grid)
    tile = tile_from_pipe(path_start_pipe)
    new_data2 = Map.put(new_data, start, tile)

    %Grid{width: grid.width, height: grid.height, data: new_data2}
  end

  def inside?(coord, %Grid{data: data} = _grid, empty) do
    if Map.has_key?(data, coord) do
      false
    else
      {coord_x, coord_y} = coord
      {start_pipe, num} =
        Enum.reduce(0..(coord_x - 1), {empty, 0}, fn x, {sp, cnt} ->
          cur = {x, coord_y}
          v = Map.get(data, cur)
          cond do
            v == @vertical -> {sp, cnt + 1}
            v == @tr -> {@tr, cnt}
            v == @br -> {@br, cnt}
            v == @tl ->
              if sp == @br do
                {empty, cnt + 1}
              else
                {sp, cnt}
              end
            v == @bl ->
              if sp == @tr do
                {empty, cnt + 1}
              else
                if sp == @br do
                  {empty, cnt}
                else
                  {sp, cnt}
                end
              end
            true -> {sp, cnt}
          end
        end)

      rem(num, 2) == 1
    end
  end

  def solve(lines) do
    grid = build_grid(lines)
    start = find_start(grid)
    path = path_finding(start, grid)
    path_grid = get_path_grid(grid, path, @empty)

    count = Enum.reduce(0..(grid.height - 1), 0, fn y, acc ->
      Enum.reduce(0..(grid.width - 1), acc, fn x, acc2 ->
        coord = {x, y}
        if inside?(coord, path_grid, @empty), do: acc2 + 1, else: acc2
      end)
    end)

    count
  end
end

defmodule Main do
  def main do
    lines = File.read!("input.txt")
    lines_list = String.split(lines, "\n", trim: true)
    count = Solver.solve(lines_list)
    IO.puts(count)
  end
end

Main.main()