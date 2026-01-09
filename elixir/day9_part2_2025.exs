
defmodule Solution do
  def main do
    points =
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line |> String.split(",") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1)
      end)
      |> Enum.filter(fn l -> length(l) == 2 end)
      |> Enum.map(fn [x, y] -> {x, y} end)

    if points == [] do
      IO.puts("No points found.")
      return()
    end

    xs = points |> Enum.map(fn {x, _} -> x end) |> Enum.uniq() |> Enum.sort()
    ys = points |> Enum.map(fn {_, y} -> y end) |> Enum.uniq() |> Enum.sort()

    x_idx = xs |> Enum.with_index() |> Map.new()
    y_idx = ys |> Enum.with_index() |> Map.new()

    nx = length(xs)
    ny = length(ys)
    w = 2 * nx + 1
    h = 2 * ny + 1

    col_w = make_dim_map(xs, nx)
    row_h = make_dim_map(ys, ny)

    to_grid = fn {x, y} -> {2 * Map.get(x_idx, x) + 1, 2 * Map.get(y_idx, y) + 1} end

    n = length(points)
    grid = Enum.reduce(0..(n - 1), %{}, fn i, acc ->
      p1 = Enum.at(points, i)
      p2 = Enum.at(points, rem(i + 1, n))
      {gx1, gy1} = to_grid.(p1)
      {gx2, gy2} = to_grid.(p2)

      if gx1 == gx2 do
        {s, e} = if gy1 <= gy2, do: {gy1, gy2}, else: {gy2, gy1}
        Enum.reduce(s..e, acc, fn y, g_acc ->
          if Map.get(row_h, y, 0) > 0, do: Map.put(g_acc, {gx1, y}, 1), else: g_acc
        end)
      else
        {s, e} = if gx1 <= gx2, do: {gx1, gx2}, else: {gx2, gx1}
        Enum.reduce(s..e, acc, fn x, g_acc ->
          if Map.get(col_w, x, 0) > 0, do: Map.put(g_acc, {x, gy1}, 1), else: g_acc
        end)
      end
    end)

    grid = bfs(:queue.from_list([{0, 0}]), Map.put(grid, {0, 0}, 2), w, h)

    pref = Enum.reduce(0..(h - 1), %{}, fn y, acc_y ->
      Enum.reduce(0..(w - 1), acc_y, fn x, acc_x ->
        val = if Map.get(grid, {x, y}) == 2, do: 0, else: Map.get(col_w, x, 0) * Map.get(row_h, y, 0)
        left = Map.get(acc_x, {x - 1, y}, 0)
        up = Map.get(acc_x, {x, y - 1}, 0)
        diag = Map.get(acc_x, {x - 1, y - 1}, 0)
        Map.put(acc_x, {x, y}, val + left + up - diag)
      end)
    end)

    sum_rect = fn {x1, y1}, {x2, y2} ->
      {xa, xb} = if x1 <= x2, do: {x1, x2}, else: {x2, x1}
      {ya, yb} = if y1 <= y2, do: {y1, y2}, else: {y2, y1}
      total = Map.get(pref, {xb, yb}, 0)
      left = Map.get(pref, {xa - 1, yb}, 0)
      up = Map.get(pref, {xb, ya - 1}, 0)
      diag = Map.get(pref, {xa - 1, ya - 1}, 0)
      total - left - up + diag
    end

    max_area = Enum.reduce(0..(n - 1), 0, fn i, max_a ->
      Enum.reduce(i..(n - 1), max_a, fn j, current_max ->
        {p1x, p1y} = Enum.at(points, i)
        {p2x, p2y} = Enum.at(points, j)
        real_w = abs(p1x - p2x) + 1
        real_h = abs(p1y - p2y) + 1
        area = real_w * real_h
        if area > current_max do
          {gx1, gy1} = to_grid.({p1x, p1y})
          {gx2, gy2} = to_grid.({p2x, p2y})
          if sum_rect.({gx1, gy1}, {gx2, gy2}) == area, do: area, else: current_max
        else
          current_max
        end
      end)
    end)

    IO.puts("Largest valid area: #{max_area}")
  end

  defp make_dim_map(coords, n) do
    map = %{0 => 1}
    Enum.reduce(Enum.with_index(coords), map, fn {v, i}, acc ->
      acc = Map.put(acc, 2 * i + 1, 1)
      gap = if i < n - 1, do: Enum.at(coords, i + 1) - v - 1, else: 1
      Map.put(acc, 2 * i + 2, if(gap > 0, do: gap, else: 0))
    end)
  end

  defp bfs(q, grid, w, h) do
    case :queue.out(q) do
      {:empty, _} -> grid
      {{:value, {x, y}}, rest} ->
        {nq, ng} = Enum.reduce([{0, 1}, {0, -1}, {1, 0}, {-1, 0}], {rest, grid}, fn {dx, dy}, {curr_q, curr_g} ->
          nx = x + dx
          ny = y + dy
          if nx >= 0 and nx < w and ny >= 0 and ny < h and Map.get(curr_g, {nx, ny}, 0) == 0 do
            {:queue.in({nx, ny}, curr_q), Map.put(curr_g, {nx, ny}, 2)}
          else
            {curr_q, curr_g}
          end
        end)
        bfs(nq, ng, w, h)
    end
  end

  defp return, do: nil
end

Solution.main()
