
defmodule Droid do
  def main do
    prog = File.read!("input.txt") |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)
    mem = Enum.with_index(prog) |> Map.new(fn {v, i} -> {i, v} end)
    grid = explore(%{mem: mem, ip: 0, rb: 0}, {0, 0}, %{{0, 0} => 1})
    {oxy, _} = Enum.find(grid, fn {_, v} -> v == 2 end)
    IO.puts bfs(:queue.from_list([{oxy, 0}]), MapSet.new([oxy]), 0, grid)
  end

  defp move({x, y}, 1), do: {x, y + 1}
  defp move({x, y}, 2), do: {x, y - 1}
  defp move({x, y}, 3), do: {x - 1, y}
  defp move({x, y}, 4), do: {x + 1, y}

  defp explore(m, pos, grid) do
    Enum.reduce(1..4, grid, fn dir, acc ->
      np = move(pos, dir)
      if Map.has_key?(acc, np), do: acc, else:
        (case step(m, [dir]) do
          {:output, 0, _} -> Map.put(acc, np, 0)
          {:output, s, nm} -> explore(nm, np, Map.put(acc, np, s))
        end)
    end)
  end

  defp bfs(q, v, mx, g) do
    case :queue.out(q) do
      {:empty, _} -> mx
      {{:value, {p, d}}, q} ->
        {nq, nv} = Enum.reduce(1..4, {q, v}, fn dir, {aq, av} ->
          np = move(p, dir)
          if Map.get(g, np, 0) != 0 and not MapSet.member?(av, np),
            do: {:queue.in({np, d + 1}, aq), MapSet.put(av, np)},
            else: {aq, av}
        end)
        bfs(nq, nv, max(mx, d), g)
    end
  end

  defp gv(m, i, md) do
    case md do
      0 -> m.mem[m.mem[i] || 0] || 0
      1 -> m.mem[i] || 0
      2 -> m.mem[m.rb + (m.mem[i] || 0)] || 0
    end
  end

  defp sv(m, i, md, v) do
    a = case md do
      0 -> m.mem[i] || 0
      2 -> m.rb + (m.mem[i] || 0)
    end
    %{m | mem: Map.put(m.mem, a, v)}
  end

  defp step(m, inputs) do
    ins = m.mem[m.ip] || 0
    {op, m1, m2, m3} = {rem(ins, 100), rem(div(ins, 100), 10), rem(div(ins, 1000), 10), rem(div(ins, 10000), 10)}
    case op do
      1 -> step(sv(m, m.ip+3, m3, gv(m, m.ip+1, m1) + gv(m, m.ip+2, m2)) |> Map.put(:ip, m.ip+4), inputs)
      2 -> step(sv(m, m.ip+3, m3, gv(m, m.ip+1, m1) * gv(m, m.ip+2, m2)) |> Map.put(:ip, m.ip+4), inputs)
      3 -> case inputs do [h|t] -> step(sv(m, m.ip+1, m1, h) |> Map.put(:ip, m.ip+2), t); [] -> {:input, m} end
      4 -> {:output, gv(m, m.ip+1, m1), %{m | ip: m.ip+2}}
      5 -> step(%{m | ip: (if gv(m, m.ip+1, m1) != 0, do: gv(m, m.ip+2, m2), else: m.ip+3)}, inputs)
      6 -> step(%{m | ip: (if gv(m, m.ip+1, m1) == 0, do: gv(m, m.ip+2, m2), else: m.ip+3)}, inputs)
      7 -> step(sv(m, m.ip+3, m3, (if gv(m, m.ip+1, m1) < gv(m, m.ip+2, m2), do: 1, else: 0)) |> Map.put(:ip, m.ip+4), inputs)
      8 -> step(sv(m, m.ip+3, m3, (if gv(m, m.ip+1, m1) == gv(m, m.ip+2, m2), do: 1, else: 0)) |> Map.put(:ip, m.ip+4), inputs)
      9 -> step(%{m | ip: m.ip+2, rb: m.rb + gv(m, m.ip+1, m1)}, inputs)
      99 -> :halt
    end
  end
end

Droid.main()

