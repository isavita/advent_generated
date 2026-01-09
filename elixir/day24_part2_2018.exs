
defmodule Solution do
  def main do
    case File.read("input.txt") do
      {:ok, input} ->
        armies = parse_input(input)
        min_boost = binary_search(armies, 0, 100_000)
        case battle(armies, min_boost) do
          {:immune_system, units} ->
            IO.puts "The smallest boost needed for the Immune System to win is: #{min_boost}"
            IO.puts "The Immune System is left with #{units} units after winning."
          _ -> :ok
        end
      _ -> :ok
    end
  end

  def parse_input(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.flat_map(fn block ->
      [header | lines] = String.split(block, "\n", trim: true)
      army = if String.contains?(header, "Immune System"), do: :immune_system, else: :infection
      Enum.with_index(lines) |> Enum.map(fn {line, i} -> parse_group(line, army, "#{army}_#{i}") end)
    end)
  end

  def parse_group(line, army, id) do
    parts = Regex.run(~r/(\d+) units each with (\d+) hit points (?:\((.*)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)/, line)
    [_, units, hp, special, damage, type, initiative] = parts
    {weak, imm} = parse_special(special)
    %{id: id, army: army, units: String.to_integer(units), hp: String.to_integer(hp),
      damage: String.to_integer(damage), type: type, initiative: String.to_integer(initiative),
      weak: MapSet.new(weak), imm: MapSet.new(imm)}
  end

  def parse_special(nil), do: {[], []}
  def parse_special(str) do
    parts = String.split(str, "; ")
    weak = Enum.find_value(parts, [], fn p -> if String.starts_with?(p, "weak to"), do: String.split(String.replace(p, "weak to ", ""), ", "), else: nil end)
    imm = Enum.find_value(parts, [], fn p -> if String.starts_with?(p, "immune to"), do: String.split(String.replace(p, "immune to ", ""), ", "), else: nil end)
    {weak, imm}
  end

  def damage_to(a, d) do
    cond do
      MapSet.member?(d.imm, a.type) -> 0
      MapSet.member?(d.weak, a.type) -> 2 * a.units * a.damage
      true -> a.units * a.damage
    end
  end

  def battle(armies, boost) do
    armies
    |> Enum.map(fn g -> if g.army == :immune_system, do: %{g | damage: g.damage + boost}, else: g end)
    |> do_fight()
  end

  def do_fight(groups) do
    targets = target_selection(groups)
    {next_groups, killed} = attack(groups, targets)
    immune_left = Enum.any?(next_groups, & &1.army == :immune_system)
    infection_left = Enum.any?(next_groups, & &1.army == :infection)
    cond do
      killed == 0 -> {:stalemate, 0}
      not infection_left -> {:immune_system, Enum.map(next_groups, & &1.units) |> Enum.sum()}
      not immune_left -> {:infection, Enum.map(next_groups, & &1.units) |> Enum.sum()}
      true -> do_fight(next_groups)
    end
  end

  def target_selection(groups) do
    groups
    |> Enum.sort_by(fn g -> {-(g.units * g.damage), -g.initiative} end)
    |> Enum.reduce({%{}, MapSet.new()}, fn att, {ts, tids} ->
      target = groups
      |> Enum.filter(fn d -> d.army != att.army and d.id not in tids end)
      |> Enum.map(fn d -> {damage_to(att, d), d.units * d.damage, d.initiative, d.id} end)
      |> Enum.filter(fn {dmg, _, _, _} -> dmg > 0 end)
      |> Enum.sort_by(fn {dmg, pwr, init, _} -> {-dmg, -pwr, -init} end)
      |> List.first()
      case target do
        {_, _, _, did} -> {Map.put(ts, att.id, did), MapSet.put(tids, did)}
        _ -> {ts, tids}
      end
    end) |> elem(0)
  end

  def attack(groups, targets) do
    order = Enum.sort_by(groups, & -&1.initiative)
    g_map = Map.new(groups, fn g -> {g.id, g} end)
    {res_map, killed} = Enum.reduce(order, {g_map, 0}, fn att_ref, {acc, k} ->
      att = Map.get(acc, att_ref.id)
      tid = Map.get(targets, att_ref.id)
      if att && att.units > 0 && tid do
        df = Map.get(acc, tid)
        dmg = damage_to(att, df)
        nk = min(div(dmg, df.hp), df.units)
        {if(df.units > nk, do: Map.put(acc, tid, %{df | units: df.units - nk}), else: Map.delete(acc, tid)), k + nk}
      else
        {acc, k}
      end
    end)
    {Map.values(res_map), killed}
  end

  def binary_search(armies, low, high) when low <= high do
    mid = div(low + high, 2)
    case battle(armies, mid) do
      {:immune_system, _} -> binary_search(armies, low, mid - 1)
      _ -> binary_search(armies, mid + 1, high)
    end
  end
  def binary_search(_, low, _), do: low
end

Solution.main()
