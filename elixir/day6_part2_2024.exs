
defmodule Solver do
  defp read_grid(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  defp find_start(grid) do
    Enum.with_index(grid)
    |> Enum.reduce(nil, fn {row, y}, acc ->
      case Enum.with_index(row) |> Enum.find(fn {cell, _} -> cell in [?^, ?>, ?v, ?<] end) do
        {cell, x} ->
          dir = case cell do
            ?^ -> 0
            ?> -> 1
            ?v -> 2
            ?< -> 3
          end
          {x, y, dir}
        _ ->
          acc
      end
    end)
  end

  defp directions, do: [{0, -1}, {1, 0}, {0, 1}, {-1, 0}]

  defp loops?(grid, {sx, sy, sdir}, max_steps \\ 2_000_000) do
    h = Enum.count(grid)
    w = Enum.count(Enum.at(grid, 0))
    dirs = directions()

    do_loops(grid, sx, sy, sdir, w, h, dirs, MapSet.new(), 0, max_steps)
  end

  defp do_loops(_grid, _x, _y, _dir, _w, _h, _dirs, seen, step, max_steps) when step >= max_steps, do: false

  defp do_loops(grid, x, y, dir, w, h, dirs, seen, step, max_steps) do
    st = {x, y, dir}

    if MapSet.member?(seen, st) do
      true
    else
      seen = MapSet.put(seen, st)
      {dx, dy} = Enum.at(dirs, dir)
      nx = x + dx
      ny = y + dy

      if nx < 0 || nx >= w || ny < 0 || ny >= h do
        false
      else
        case Enum.at(Enum.at(grid, ny), nx) do
          ?# ->
            do_loops(grid, x, y, rem(dir + 1, 4), w, h, dirs, seen, step + 1, max_steps)

          _ ->
            do_loops(grid, nx, ny, dir, w, h, dirs, seen, step + 1, max_steps)
        end
      end
    end
  end

  def solve(path \\ "input.txt") do
    grid = read_grid(path)
    {start_x, start_y, start_dir} = find_start(grid)

    grid = List.update_at(grid, start_y, fn row -> List.replace_at(row, start_x, ?.) end)

        h = Enum.count(grid)
        w = Enum.count(Enum.at(grid, 0))
    0..(h - 1)
    |> Enum.reduce(0, fn y, acc ->
      0..(w - 1)
      |> Enum.reduce(acc, fn x, inner_acc ->
        if x == start_x && y == start_y do
          inner_acc
        else
            case Enum.at(Enum.at(grid,y),x) do
              ?. ->
                new_grid = List.update_at(grid, y, fn row -> List.replace_at(row, x, ?#) end)

                if loops?(new_grid, {start_x, start_y, start_dir}, 20000) do
                  inner_acc + 1
                else
                  inner_acc
                end
              _ -> inner_acc
            end
        end
      end)
    end)
    |> IO.puts()
  end
end

Solver.solve()
