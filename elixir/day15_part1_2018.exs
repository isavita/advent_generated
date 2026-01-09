
defmodule Solution do
  def main do
    state = parse_input("input.txt")
    IO.puts(solve(state))
  end

  defp parse_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(%{walls: MapSet.new(), units: %{}, round: 0}, fn {line, y}, acc ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, x}, acc_inner ->
        case char do
          ?# -> %{acc_inner | walls: MapSet.put(acc_inner.walls, {x, y})}
          ?E -> put_in(acc_inner, [:units, {x, y}], %{type: :elf, hp: 200, power: 3})
          ?G -> put_in(acc_inner, [:units, {x, y}], %{type: :goblin, hp: 200, power: 3})
          _ -> acc_inner
        end
      end)
    end)
  end

  defp solve(state) do
    case tick(state) do
      {:ok, next_state} -> solve(next_state)
      {:halt, final_state} -> 
        final_state.round * (final_state.units |> Map.values() |> Enum.map(& &1.hp) |> Enum.sum())
    end
  end

  defp tick(state) do
    state.units
    |> Map.keys()
    |> Enum.sort_by(fn {x, y} -> {y, x} end)
    |> process_units(state)
  end

  defp process_units([], state), do: {:ok, update_in(state.round, &(&1 + 1))}
  defp process_units([pos | rest], state) do
    case state.units[pos] do
      nil -> process_units(rest, state)
      unit ->
        enemies = Enum.filter(state.units, fn {_, u} -> u.type != unit.type end)
        if Enum.empty?(enemies) do
          {:halt, state}
        else
          {new_pos, state} = move(pos, unit, state)
          new_unit = state.units[new_pos]
          state = attack(new_pos, new_unit, state)
          process_units(rest, state)
        end
    end
  end

  defp move(pos, unit, state) do
    if Enum.any?(neighbors(pos), fn n -> Map.has_key?(state.units, n) and state.units[n].type != unit.type end) do
      {pos, state}
    else
      targets = state.units
                |> Enum.filter(fn {_, u} -> u.type != unit.type end)
                |> Enum.flat_map(fn {epos, _} -> neighbors(epos) end)
                |> Enum.filter(fn n -> !MapSet.member?(state.walls, n) and !Map.has_key?(state.units, n) end)
                |> Enum.uniq()

      dists = bfs(pos, state.walls, state.units)
      reachable = Enum.filter(targets, &Map.has_key?(dists, &1))

      if Enum.empty?(reachable) do
        {pos, state}
      else
        min_d = reachable |> Enum.map(&dists[&1]) |> Enum.min()
        best_t = reachable |> Enum.filter(&(dists[&1] == min_d)) |> Enum.sort_by(fn {tx, ty} -> {ty, tx} end) |> List.first()
        
        dists_from_t = bfs(best_t, state.walls, Map.delete(state.units, pos))
        step = neighbors(pos)
               |> Enum.filter(&Map.has_key?(dists_from_t, &1))
               |> Enum.sort_by(fn {nx, ny} -> {dists_from_t[{nx, ny}], ny, nx} end)
               |> List.first()

        {step, %{state | units: state.units |> Map.delete(pos) |> Map.put(step, unit)}}
      end
    end
  end

  defp attack(pos, unit, state) do
    target = neighbors(pos)
             |> Enum.filter(fn n -> Map.has_key?(state.units, n) and state.units[n].type != unit.type end)
             |> Enum.sort_by(fn {nx, ny} -> {state.units[{nx, ny}].hp, ny, nx} end)
             |> List.first()

    if target do
      new_hp = state.units[target].hp - unit.power
      if new_hp <= 0 do
        %{state | units: Map.delete(state.units, target)}
      else
        put_in(state, [:units, target, :hp], new_hp)
      end
    else
      state
    end
  end

  defp neighbors({x, y}), do: [{x, y - 1}, {x - 1, y}, {x + 1, y}, {x, y + 1}]

  defp bfs(start, walls, units) do
    do_bfs(:queue.from_list([{start, 0}]), %{start => 0}, walls, units)
  end

  defp do_bfs(q, visited, walls, units) do
    case :queue.out(q) do
      {{:value, {pos, d}}, q} ->
        ns = neighbors(pos) |> Enum.filter(fn n -> !MapSet.member?(walls, n) and !Map.has_key?(units, n) and !Map.has_key?(visited, n) end)
        {nq, nv} = Enum.reduce(ns, {q, visited}, fn n, {aq, av} -> {:queue.in({n, d + 1}, aq), Map.put(av, n, d + 1)} end)
        do_bfs(nq, nv, walls, units)
      {:empty, _} -> visited
    end
  end
end

Solution.main()

