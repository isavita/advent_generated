
defmodule Solver do
  @dirs {{-1, 0}, {0, 1}, {1, 0}, {0, -1}}

  def main do
    grid = for {l, r} <- Enum.with_index(File.read!("input.txt") |> String.split("\n", trim: true)),
               {char, c} <- Enum.with_index(String.to_charlist(l)), into: %{}, do: {{r, c}, char}
    {sx, sy} = Enum.find_value(grid, fn {k, v} -> if v == ?S, do: k end)
    {ex, ey} = Enum.find_value(grid, fn {k, v} -> if v == ?E, do: k end)
    dists = solve_pq(:gb_sets.singleton({0, sx, sy, 1}), %{{sx, sy, 1} => 0}, grid, {ex, ey})
    best = 0..3 |> Enum.map(&Map.get(dists, {ex, ey, &1}, :infinity)) |> Enum.min()
    q = for d <- 0..3, Map.get(dists, {ex, ey, d}) == best, do: {ex, ey, d}
    IO.puts backtrack(q, dists, MapSet.new(), MapSet.new())
  end

  defp solve_pq(pq, dists, grid, target) do
    if :gb_sets.is_empty(pq) do dists else
      {{c, r, c_idx, d}, pq} = :gb_sets.take_smallest(pq)
      if c > Map.get(dists, {r, c_idx, d}, :infinity) or {r, c_idx} == target do
        solve_pq(pq, dists, grid, target)
      else
        {dr, dc} = elem(@dirs, d)
        moves = [{{r, c_idx, rem(d + 1, 4)}, c + 1000}, {{r, c_idx, rem(d + 3, 4)}, c + 1000}]
        moves = if grid[{r + dr, c_idx + dc}] != ?#, do: [{{r + dr, c_idx + dc, d}, c + 1} | moves], else: moves
        {pq, dists} = Enum.reduce(moves, {pq, dists}, fn {{nr, nc, nd}, nco}, {aq, ad} ->
          if nco < Map.get(ad, {nr, nc, nd}, :infinity), do: {:gb_sets.add_element({nco, nr, nc, nd}, aq), Map.put(ad, {nr, nc, nd}, nco)}, else: {aq, ad}
        end)
        solve_pq(pq, dists, grid, target)
      end
    end
  end

  defp backtrack(stack, dists, vis, coords) do
    case stack do
      [] -> MapSet.size(coords)
      [{r, c, d} = state | rest] ->
        if MapSet.member?(vis, state) do backtrack(rest, dists, vis, coords) else
          {dr, dc} = elem(@dirs, d)
          cost = Map.get(dists, state)
          candidates = [{r, c, rem(d + 1, 4), 1000}, {r, c, rem(d + 3, 4), 1000}, {r - dr, c - dc, d, 1}]
          new_stack = Enum.reduce(candidates, rest, fn {pr, pc, pd, diff}, acc ->
            if Map.get(dists, {pr, pc, pd}) == cost - diff, do: [{pr, pc, pd} | acc], else: acc
          end)
          backtrack(new_stack, dists, MapSet.put(vis, state), MapSet.put(coords, {r, c}))
        end
    end
  end
end

Solver.main()
