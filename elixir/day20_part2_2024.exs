
defmodule Solver do
  defp read_grid(filename) do
    {:ok, contents} = File.read(filename)

    contents
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  defp find_positions(grid) do
    h = length(grid)
    w = length(Enum.at(grid, 0))
    {s, e, walls, track_cells} = find_positions_recursive(grid, 0, 0, {nil, nil, MapSet.new(), []}, h, w)
    {s, e, MapSet.to_list(walls), track_cells}
  end
  defp find_positions_recursive([], _row, _col, acc, _h, _w), do: acc
  defp find_positions_recursive([row | rest_rows], row_index, _col, {s, e, walls, track_cells}, h, w) do
      {s_new, e_new, walls_new, track_cells_new} = find_in_row(row, row_index, 0, {s, e, walls, track_cells}, w)
      find_positions_recursive(rest_rows, row_index + 1, 0, {s_new, e_new, walls_new, track_cells_new}, h, w)

  end

  defp find_in_row([], _row, _col, acc, _w), do: acc
  defp find_in_row([char|rest], row, col, {s, e, walls, track_cells}, w) do
      new_s = if char == ?S, do: {row, col}, else: s
      new_e = if char == ?E, do: {row, col}, else: e
      new_walls = if char == ?#, do: MapSet.put(walls, {row, col}), else: walls

      new_track_cells = if char != ?#, do: [{row, col} | track_cells] , else: track_cells
      find_in_row(rest, row, col+1, {new_s, new_e, new_walls, new_track_cells}, w)
  end

  defp is_track(x, y, h, w, walls) do
    x >= 0 && x < h && y >= 0 && y < w && not Enum.member?(walls, {x, y})
  end

  defp normal_dist_from(start, h, w, walls, track_cells) do
      track_cells
      |> Enum.map(fn({i,j})-> {{i,j},-1} end)
      |> Map.new
      |> then(fn(m)-> Map.put(m,start,0) end)
      |> bfs(start, h, w, walls, [])
  end

  defp bfs(dist, current, h, w, walls, queue) do
    case queue do
      [] ->
        case current do
          nil -> dist
          _ ->
              dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]
              {x,y} = current
              current_dist = Map.get(dist,current)
              new_queue =
                for {dx, dy} <- dirs,
                  nx = x + dx,
                  ny = y + dy,
                  is_track(nx, ny, h, w, walls),
                  Map.get(dist, {nx, ny}) == -1,
                    do: {nx, ny}

              new_dist = Enum.reduce(new_queue,dist, fn(pos,acc_dist)->
                Map.put(acc_dist,pos,current_dist+1)
              end)
              bfs(new_dist, nil, h, w, walls, new_queue)
        end
      [head | tail] ->
          bfs(dist, head, h, w, walls, tail)
    end
  end

  defp calculate_cheats(dist_from_s, dist_from_e, h, w, track_cells, normal_cost) do

    track_cells
    |> Enum.reduce(Map.new(), fn start_pos, acc_cheats ->
      sd = Map.get(dist_from_s, start_pos)

      if sd != nil and sd >= 0 do
        bfs_cheat(start_pos, h, w, 20)
        |> Enum.reduce(acc_cheats, fn {end_pos, s}, inner_acc ->
          if s > 0 and s <= 20 do
            ed = Map.get(dist_from_e, end_pos)

            if ed != nil and ed >=0 do
              cost = sd + s + ed

              if cost < normal_cost do
                Map.update(inner_acc, {start_pos, end_pos}, cost, &min(&1, cost))
              else
                inner_acc
              end
            else
              inner_acc
            end
          else
            inner_acc
          end
        end)
      else
        acc_cheats
      end
    end)
  end

  defp bfs_cheat(start_pos, h, w, max_steps) do
        q = [start_pos]
        dist_c = %{start_pos => 0}

        bfs_cheat_recursive(q,dist_c, h, w, max_steps)
  end
  defp bfs_cheat_recursive([], dist_c, _h, _w, _max_steps), do: dist_c
  defp bfs_cheat_recursive(q, dist_c, h, w, max_steps) do
      case q do
          [] -> dist_c
          [current | tail] ->
            steps = Map.get(dist_c, current)
            if steps == max_steps do
                bfs_cheat_recursive(tail,dist_c,h,w, max_steps)
            else
              dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]
              {x,y} = current

              new_q =
                for {dx, dy} <- dirs,
                  nx = x + dx,
                  ny = y + dy,
                  nx >= 0 and nx < h and ny >=0 and ny < w,
                  not Map.has_key?(dist_c,{nx,ny}) do
                    {nx,ny}
                end

              new_dist_c = Enum.reduce(new_q,dist_c,fn(pos,acc)->
                Map.put(acc,pos,steps+1)
              end)

              bfs_cheat_recursive(tail++new_q, new_dist_c, h, w, max_steps)
            end
      end
  end

  def solve(filename) do
    grid = read_grid(filename)
    h = length(grid)
    w = length(Enum.at(grid, 0))

    {s, e, walls, track_cells} = find_positions(grid)

    dist_from_s = normal_dist_from(s, h, w, walls, track_cells)
    dist_from_e = normal_dist_from(e, h, w, walls, track_cells)
    {ex,ey} = e
    normal_cost = Map.get(dist_from_s, {ex, ey})

    if normal_cost == nil or normal_cost<0 do
      IO.puts(0)
    else
      cheats = calculate_cheats(dist_from_s, dist_from_e, h, w, track_cells, normal_cost)

      cheats
      |> Map.values
      |> Enum.count(&(normal_cost - &1 >= 100))
      |> IO.puts
    end
  end
end

Solver.solve("input.txt")
