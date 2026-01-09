
defmodule Solver do
  def main do
    input = File.read!("input.txt") |> String.trim()
    lines = String.split(input, "\n")
    h = length(lines)
    w = String.length(hd(lines))
    grid = for {l, y} <- Enum.with_index(lines), {c, x} <- Enum.with_index(String.to_charlist(l)), c != ?., into: %{}, do: {{x, y}, c}
    period = div((w - 2) * (h - 2), Integer.gcd(w - 2, h - 2))
    bfs(:queue.from_list([{{1, 0}, 0}]), MapSet.new([{{1, 0}, 0}]), grid, w, h, {w - 2, h - 1}, period)
  end

  def bfs(q, seen, grid, w, h, goal, period) do
    case :queue.out(q) do
      {{:value, {{cx, cy}, t}}, q} ->
        if {cx, cy} == goal do
          IO.puts t
        else
          nt = t + 1
          {nq, ns} = Enum.reduce([{1, 0}, {-1, 0}, {0, 1}, {0, -1}, {0, 0}], {q, seen}, fn {dx, dy}, {aq, as} ->
            {nx, ny} = {cx + dx, cy + dy}
            key = {{nx, ny}, rem(nt, period)}
            if nx >= 0 and nx < w and ny >= 0 and ny < h and grid[{nx, ny}] != ?# and !MapSet.member?(as, key) and
               (ny <= 0 or ny >= h - 1 or !bliz?(nx, ny, nt, grid, w, h)) do
              {:queue.in({{nx, ny}, nt}, aq), MapSet.put(as, key)}
            else
              {aq, as}
            end
          end)
          bfs(nq, ns, grid, w, h, goal, period)
        end
      {:empty, _} -> :error
    end
  end

  def bliz?(x, y, t, grid, w, h) do
    grid[{Integer.mod(x - 1 - t, w - 2) + 1, y}] == ?> or
    grid[{Integer.mod(x - 1 + t, w - 2) + 1, y}] == ?< or
    grid[{x, Integer.mod(y - 1 - t, h - 2) + 1}] == ?v or
    grid[{x, Integer.mod(y - 1 + t, h - 2) + 1}] == ?^
  end
end

Solver.main()
