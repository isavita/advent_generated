
defmodule Solver do
  use Bitwise

  def main do
    grid = File.read!("input.txt") |> String.split("\n", trim: true)
    map = for {l, y} <- Enum.with_index(grid), {c, x} <- Enum.with_index(String.to_charlist(l)), into: %{}, do: {{x, y}, c}
    start = Enum.find_value(map, fn {p, v} -> if v == ?@, do: p end)
    target = Enum.reduce(map, 0, fn {_, v}, a -> if v >= ?a and v <= ?z, do: a ||| (1 <<< (v - ?a)), else: a end)
    m = if map[start] >= ?a and map[start] <= ?z, do: 1 <<< (map[start] - ?a), else: 0
    IO.puts solve(:queue.from_list([{start, m, 0}]), MapSet.new([{start, m}]), map, target)
  end

  defp solve(q, v, map, target) do
    case :queue.out(q) do
      {:empty, _} -> "Infinity"
      {{:value, {{x, y}, m, d}}, q} ->
        if m == target do
          d
        else
          {nq, nv} = Enum.reduce([{0,1},{0,-1},{1,0},{-1,0}], {q, v}, fn {dx, dy}, {aq, av} ->
            np = {x + dx, y + dy}
            c = map[np]
            if c && c != ?# and can_p(c, m) do
              nm = if c >= ?a and c <= ?z, do: m ||| (1 <<< (c - ?a)), else: m
              if {np, nm} not in av, do: {:queue.in({np, nm, d + 1}, aq), MapSet.put(av, {np, nm})}, else: {aq, av}
            else
              {aq, av}
            end
          end)
          solve(nq, nv, map, target)
        end
    end
  end

  defp can_p(c, m) when c >= ?A and c <= ?Z, do: (m &&& (1 <<< (c - ?A))) != 0
  defp can_p(_, _), do: true
end

Solver.main()
