
defmodule Day15 do
  def main do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    {power, outcome} = find_min_power(input, 3, 200)
    IO.puts "Minimum Elf attack power: #{power}"
    IO.puts "Outcome: #{outcome}"
  end

  def find_min_power(input, low, high) do
    if low >= high do
      {outcome, _} = combat(input, low, true)
      {low, outcome}
    else
      mid = div(low + high, 2)
      {_, all_saved} = combat(input, mid, true)
      if all_saved, do: find_min_power(input, low, mid), else: find_min_power(input, mid + 1, high)
    end
  end

  def combat(input, elf_power, stop_on_elf_death) do
    {grid, units} = parse(input, elf_power)
    initial_elves = units |> Map.values() |> Enum.count(&(&1.type == :elf))
    run_simulation(0, units, grid, initial_elves, stop_on_elf_death)
  end

  def parse(input, elf_power) do
    grid = for {line, y} <- Enum.with_index(input), {char, x} <- Enum.with_index(String.graphemes(line)), into: %{}, do: {{x, y}, (if char == "#", do: :wall, else: :space)}
    units = for {line, y} <- Enum.with_index(input), {char, x} <- Enum.with_index(String.graphemes(line)), char in ["E", "G"], into: %{} do
      {make_ref(), %{pos: {x, y}, type: (if char == "E", do: :elf, else: :goblin), hp: 200, power: (if char == "E", do: elf_power, else: 3)}}
    end
    {grid, units}
  end

  def run_simulation(round, units, grid, initial_elves, stop_on_elf_death) do
    sorted_ids = Map.to_list(units) |> Enum.sort_by(fn {_, u} -> {elem(u.pos, 1), elem(u.pos, 0)} end) |> Enum.map(&elem(&1, 0))
    case process_turns(sorted_ids, units, grid, stop_on_elf_death) do
      {:ok, next_units} -> run_simulation(round + 1, next_units, grid, initial_elves, stop_on_elf_death)
      {:combat_ended, final_units} -> {round * (final_units |> Map.values() |> Enum.map(& &1.hp) |> Enum.sum()), Enum.count(Map.values(final_units), &(&1.type == :elf)) == initial_elves}
      {:elf_died, _} -> {0, false}
    end
  end

  def process_turns(ids, units, grid, stop_on_elf_death) do
    Enum.reduce_while(ids, {:ok, units}, fn id, {:ok, acc_units} ->
      if not Map.has_key?(acc_units, id) do
        {:cont, {:ok, acc_units}}
      else
        unit = acc_units[id]
        enemies = Enum.filter(acc_units, fn {_, u} -> u.type != unit.type end)
        if enemies == [] do
          {:halt, {:combat_ended, acc_units}}
        else
          {new_pos, moved_units} = move(id, unit, acc_units, grid)
          {final_units, elf_died} = attack(id, new_pos, moved_units)
          if elf_died and stop_on_elf_death, do: {:halt, {:elf_died, final_units}}, else: {:cont, {:ok, final_units}}
        end
      end
    end)
  end

  def move(id, unit, units, grid) do
    pos = unit.pos
    if is_adjacent_to_enemy?(pos, unit.type, units) do
      {pos, units}
    else
      unit_positions = units |> Map.values() |> Enum.map(& &1.pos) |> MapSet.new()
      dist_from_pos = bfs(pos, grid, unit_positions)
      targets = units |> Map.values() |> Enum.filter(&(&1.type != unit.type)) |> Enum.flat_map(&(get_adj(&1.pos)))
        |> Enum.filter(&(grid[&1] == :space and not MapSet.member?(unit_positions, &1))) |> Enum.uniq()
      reachable = targets |> Enum.filter(&Map.has_key?(dist_from_pos, &1))
      if reachable == [] do
        {pos, units}
      else
        chosen_target = Enum.min_by(reachable, fn {tx, ty} -> {dist_from_pos[{tx, ty}], ty, tx} end)
        dist_from_target = bfs(chosen_target, grid, MapSet.delete(unit_positions, pos))
        next_step = get_adj(pos) |> Enum.filter(&(grid[&1] == :space and not MapSet.member?(unit_positions, &1) and Map.has_key?(dist_from_target, &1)))
          |> Enum.min_by(fn {nx, ny} -> {dist_from_target[{nx, ny}], ny, nx} end)
        {next_step, put_in(units, [id, :pos], next_step)}
      end
    end
  end

  def attack(id, pos, units) do
    unit = units[id]
    adjs = get_adj(pos)
    adj_enemies = units |> Enum.filter(fn {_, u} -> u.type != unit.type and u.pos in adjs end)
    if adj_enemies == [] do
      {units, false}
    else
      {target_id, target} = Enum.min_by(adj_enemies, fn {_, u} -> {u.hp, elem(u.pos, 1), elem(u.pos, 0)} end)
      new_hp = target.hp - unit.power
      if new_hp <= 0, do: {Map.delete(units, target_id), target.type == :elf}, else: {put_in(units, [target_id, :hp], new_hp), false}
    end
  end

  def is_adjacent_to_enemy?(pos, type, units) do
    adjs = get_adj(pos)
    Enum.any?(units, fn {_, u} -> u.type != type and u.pos in adjs end)
  end

  def get_adj({x, y}), do: [{x, y - 1}, {x - 1, y}, {x + 1, y}, {x, y + 1}]

  def bfs(start_pos, grid, unit_positions) do
    iterate_bfs(:queue.from_list([{start_pos, 0}]), grid, unit_positions, %{start_pos => 0})
  end

  def iterate_bfs(q, grid, unit_positions, visited) do
    case :queue.out(q) do
      {:empty, _} -> visited
      {{:value, {pos, d}}, q} ->
        {new_q, new_v} = Enum.reduce(get_adj(pos), {q, visited}, fn adj, {acc_q, acc_v} ->
          if Map.get(grid, adj) == :space and not MapSet.member?(unit_positions, adj) and not Map.has_key?(acc_v, adj) do
            {:queue.in({adj, d + 1}, acc_q), Map.put(acc_v, adj, d + 1)}
          else
            {acc_q, acc_v}
          end
        end)
        iterate_bfs(new_q, grid, unit_positions, new_v)
    end
  end
end

Day15.main()
