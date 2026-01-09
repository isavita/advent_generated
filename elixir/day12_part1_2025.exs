
import Bitwise

defmodule Solver do
  def normalize(pts) do
    if pts == [], do: [], else: (
      min_r = pts |> Enum.map(&elem(&1, 0)) |> Enum.min()
      pts_at_min_r = pts |> Enum.filter(&(elem(&1, 0) == min_r))
      min_c_at_min_r = pts_at_min_r |> Enum.map(&elem(&1, 1)) |> Enum.min()
      pts |> Enum.map(fn {r, c} -> {r - min_r, c - min_c_at_min_r} end) |> Enum.sort()
    )
  end

  def variations(pts) do
    for i <- 0..3, flip <- [false, true], reduce: MapSet.new() do
      acc ->
        p = if flip, do: Enum.map(pts, fn {r, c} -> {r, -c} end), else: pts
        p = Enum.reduce(if(i > 0, do: 1..i, else: []), p, fn _, ap -> Enum.map(ap, fn {r, c} -> {c, -r} end) end)
        MapSet.put(acc, normalize(p))
    end |> MapSet.to_list()
  end

  def flood_fill(start, rows, cols, grid, mask) do
    do_flood([start], 0, rows, cols, grid, mask ||| (1 <<< start))
  end
  defp do_flood([], size, _, _, _, mask), do: {size, mask}
  defp do_flood([h | t], size, rows, cols, grid, mask) do
    r = div(h, cols); c = rem(h, cols)
    {nq, nm} = Enum.reduce([{r-1, c}, {r+1, c}, {r, c-1}, {r, c+1}], {t, mask}, fn {nr, nc}, {qa, ma} ->
      idx = nr * cols + nc
      if nr >= 0 and nr < rows and nc >= 0 and nc < cols and elem(grid, idx) == 0 and (ma &&& (1 <<< idx)) == 0 do
        {[idx | qa], ma ||| (1 <<< idx)}
      else
        {qa, ma}
      end
    end)
    do_flood(nq, size + 1, rows, cols, grid, nm)
  end

  def check_islands(rows, cols, grid, counts, slack_idx, shapes) do
    min_real = counts |> Enum.reject(fn {id, cnt} -> id == slack_idx or cnt == 0 end)
               |> Enum.map(fn {id, _} -> length(shapes[id]) end)
               |> (fn [] -> nil; l -> Enum.min(l) end).()
    if is_nil(min_real) do
      true
    else
      avail = Map.get(counts, slack_idx, 0)
      {_, final_avail} = Enum.reduce(0..(rows * cols - 1), {0, avail}, fn i, {m, av} ->
        if av < 0 or elem(grid, i) == 1 or (m &&& (1 <<< i)) != 0, do: {m, av}, else: (
          {sz, nm} = flood_fill(i, rows, cols, grid, m)
          if sz < min_real, do: {nm, av - sz}, else: {nm, av}
        )
      end)
      final_avail >= 0
    end
  end

  def solve(rows, cols, grid, counts, ids, vars, slack_idx, shapes) do
    size = rows * cols
    case Enum.find(0..size-1, &(elem(grid, &1) == 0)) do
      nil -> true
      f_idx ->
        if check_islands(rows, cols, grid, counts, slack_idx, shapes) do
          Enum.any?(ids, fn id ->
            if counts[id] > 0 do
              new_counts = Map.put(counts, id, counts[id] - 1)
              Enum.any?(vars[id], fn var ->
                if can_place?(grid, rows, cols, f_idx, var) do
                  solve(rows, cols, place(grid, f_idx, var, cols), new_counts, ids, vars, slack_idx, shapes)
                end
              end)
            end
          end)
        else
          false
        end
    end
  end

  defp can_place?(grid, rows, cols, f_idx, var) do
    Enum.all?(var, fn {dr, dc} ->
      nr = div(f_idx, cols) + dr
      nc = rem(f_idx, cols) + dc
      nr >= 0 and nr < rows and nc >= 0 and nc < cols and elem(grid, nr * cols + nc) == 0
    end)
  end

  defp place(grid, f_idx, var, cols) do
    Enum.reduce(var, grid, fn {dr, dc}, acc ->
      idx = (div(f_idx, cols) + dr) * cols + (rem(f_idx, cols) + dc)
      put_elem(acc, idx, 1)
    end)
  end

  def main do
    lines = File.read!("input.txt") |> String.split("\n") |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == ""))
    {shapes_raw, region_lines, _, _} = Enum.reduce(lines, {%{}, [], nil, true}, fn line, {s_acc, r_acc, c_id, parsing} ->
      cond do
        parsing and String.contains?(line, "x") and String.contains?(line, ":") -> {s_acc, [line | r_acc], c_id, false}
        not parsing -> {s_acc, [line | r_acc], c_id, false}
        String.ends_with?(line, ":") ->
          id = String.to_integer(String.slice(line, 0..-2))
          {Map.put(s_acc, id, []), r_acc, id, true}
        true -> {Map.update!(s_acc, c_id, &(&1 ++ [line])), r_acc, c_id, true}
      end
    end)
    shapes = Enum.reduce(shapes_raw, %{}, fn {id, rows}, acc ->
      pts = for {row, r} <- Enum.with_index(rows), {char, c} <- Enum.with_index(String.graphemes(row)), char == "#", do: {r, c}
      Map.put(acc, id, normalize(pts))
    end)
    max_id = Map.keys(shapes) |> Enum.max(fn -> -1 end)
    slack_idx = max_id + 1
    shapes = Map.put(shapes, slack_idx, [{0, 0}])
    vars = for {id, pts} <- shapes, into: %{}, do: {id, variations(pts)}
    
    solved = Enum.reduce(Enum.reverse(region_lines), 0, fn line, acc_solved ->
      [dim, cnt_str] = String.split(line, ":", parts: 2)
      [w, h] = String.split(dim, "x") |> Enum.map(&String.to_integer/1)
      tokens = String.split(cnt_str, ~r/\s+/, trim: true)
      {pc, tp} = Enum.reduce(Enum.with_index(tokens), {%{}, 0}, fn {tok, idx}, {pca, ta} ->
        c = String.to_integer(tok)
        if c > 0 and Map.has_key?(shapes, idx), do: {Map.put(pca, idx, c), ta + c * length(shapes[idx])}, else: {pca, ta}
      end)
      if tp <= w * h do
        slack = w * h - tp
        pc = if slack > 0, do: Map.put(pc, slack_idx, slack), else: pc
        ids = Map.keys(pc) |> Enum.sort_by(fn id -> -length(shapes[id]) end)
        grid = List.to_tuple(List.duplicate(0, w * h))
        if solve(h, w, grid, pc, ids, vars, slack_idx, shapes), do: acc_solved + 1, else: acc_solved
      else
        acc_solved
      end
    end)
    IO.puts "Number of regions that fit all presents: #{solved}"
  end
end

Solver.main()
