
defmodule Day24 do
  defstruct [:id, :army, :units, :hp, :damage, :type, :initiative, :immunities, :weaknesses]

  def run do
    input =
      "input.txt"
      |> File.read!()
      |> String.trim()
      |> String.split("\n")

    {groups, _} = parse(input)
    groups |> battle() |> Enum.map(& &1.units) |> Enum.sum() |> IO.puts()
  end

  defp parse(lines) do
    army_map = %{"Immune System" => :immune, "Infection" => :infection}
    re_army = ~r/^(.*):$/
    re_desc = ~r/^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/
    re_imm = ~r/immune to ([^;)]*)/
    re_weak = ~r/weak to ([^;)]*)/

    {groups, _id, _army} =
      Enum.reduce(lines, {[], 1, nil}, fn line, {gs, id, cur} ->
        cond do
          Regex.run(re_army, line) ->
            [_, name] = Regex.run(re_army, line)
            {gs, id, Map.get(army_map, name)}

          true ->
            case Regex.run(re_desc, line) do
              nil -> {gs, id, cur}
              [_ | [u, hp, dmg, type, init]] ->
                imm = case Regex.run(re_imm, line) do
                  nil -> []; [_ , list] -> String.split(list, ~r/, /, trim: true)
                end

                weak = case Regex.run(re_weak, line) do
                  nil -> []; [_ , list] -> String.split(list, ~r/, /, trim: true)
                end

                group = %__MODULE__{
                  id: id,
                  army: cur,
                  units: String.to_integer(u),
                  hp: String.to_integer(hp),
                  damage: String.to_integer(dmg),
                  type: type,
                  initiative: String.to_integer(init),
                  immunities: imm,
                  weaknesses: weak
                }

                {[group | gs], id + 1, cur}
            end
        end
      end)

    {Enum.reverse(groups), %{}}
  end

  defp battle(groups) do
    groups_map = Map.new(groups, &{&1.id, &1})
    loop(groups_map)
  end

  defp loop(groups) do
    if finished?(groups), do: Map.values(groups), else: loop(step(groups))
  end

  defp finished?(groups) do
    armies = groups |> Map.values() |> Enum.map(& &1.army) |> Enum.uniq()
    length(armies) == 1
  end

  defp step(groups) do
    target_sel = select_targets(groups)
    attack_order = groups |> Map.values() |> Enum.sort_by(& &1.initiative, :desc)

    groups =
      Enum.reduce(attack_order, groups, fn attacker, acc ->
        case Map.get(target_sel, attacker.id) do
          nil -> acc
          target_id ->
            case {Map.get(acc, attacker.id), Map.get(acc, target_id)} do
              {%{units: a_units} = a, %{units: t_units} = t} when a_units > 0 and t_units > 0 ->
                dmg = damage(a, t)
                killed = div(dmg, t.hp)
                new_units = max(t_units - killed, 0)
                Map.update!(acc, target_id, &%{&1 | units: new_units})
              _ -> acc
            end
        end
      end)

    groups |> Enum.reject(fn {_id, g} -> g.units <= 0 end) |> Map.new()
  end

  defp select_targets(groups) do
    order =
      groups
      |> Map.values()
      |> Enum.sort_by(fn g -> {-effective_power(g), -g.initiative} end)

    Enum.reduce(order, {%{}, MapSet.new()}, fn attacker, {sel, taken} ->
      enemy_army = opposite(attacker.army)

      candidates =
        groups
        |> Map.values()
        |> Enum.filter(fn g ->
          g.army == enemy_army and g.units > 0 and not MapSet.member?(taken, g.id)
        end)

      {target, _} =
        Enum.reduce(candidates, {nil, 0}, fn enemy, {best, best_val} ->
          dmg = damage(attacker, enemy)

          cond do
            dmg == 0 -> {best, best_val}
            dmg > best_val -> {enemy, dmg}
            dmg == best_val and effective_power(enemy) > effective_power(best || enemy) -> {enemy, dmg}
            dmg == best_val and effective_power(enemy) == effective_power(best || enemy) and enemy.initiative > (best && best.initiative || -1) ->
              {enemy, dmg}
            true -> {best, best_val}
          end
        end)

      if target do
        {Map.put(sel, attacker.id, target.id), MapSet.put(taken, target.id)}
      else
        {sel, taken}
      end
    end)
    |> elem(0)
  end

  defp opposite(:immune), do: :infection
  defp opposite(:infection), do: :immune

  defp effective_power(g), do: g.units * g.damage

  defp damage(att, def) do
    cond do
      att.type in def.immunities -> 0
      att.type in def.weaknesses -> effective_power(att) * 2
      true -> effective_power(att)
    end
  end
end

Day24.run()
