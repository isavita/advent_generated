
defmodule RPGSimulator do
  @weapons [
    {8, 4, 0},   # {cost, damage, armor}
    {10, 5, 0},
    {25, 6, 0},
    {40, 7, 0},
    {74, 8, 0}
  ]

  @armor [
    {13, 0, 1},
    {31, 0, 2},
    {53, 0, 3},
    {75, 0, 4},
    {102, 0, 5}
  ]

  @rings [
    {25, 1, 0},
    {50, 2, 0},
    {100, 3, 0},
    {20, 0, 1},
    {40, 0, 2},
    {80, 0, 3}
  ]

  @player_hp 100

  def run do
    {boss_hp, boss_damage, boss_armor} = read_boss_stats("input.txt")
    min_cost = find_min_cost(boss_hp, boss_damage, boss_armor)
    IO.puts("Minimum cost to win: #{min_cost}")
  end

  defp read_boss_stats(file) do
    File.read!(file)
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_boss_line/1)
    |> List.to_tuple()
  end

  defp parse_boss_line(line) do
    case String.split(line, ": ") do
      ["Hit Points", hp] -> String.to_integer(hp)
      ["Damage", damage] -> String.to_integer(damage)
      ["Armor", armor] -> String.to_integer(armor)
    end
  end

  defp find_min_cost(boss_hp, boss_damage, boss_armor) do
    for weapon <- @weapons, armor <- [nil | @armor], rings <- combinations(@rings, 2) do
      {weapon_cost, weapon_damage, weapon_armor} = weapon
      {armor_cost, armor_damage, armor_armor} = armor || {0, 0, 0}

      total_damage = weapon_damage + Enum.sum(Enum.map(rings, fn {_, d, _} -> d end))
      total_armor = weapon_armor + armor_armor + Enum.sum(Enum.map(rings, fn {_, _, a} -> a end))

      total_cost = weapon_cost + armor_cost + Enum.sum(Enum.map(rings, fn {cost, _, _} -> cost end))

      if player_wins?(@player_hp, total_damage, total_armor, boss_hp, boss_damage, boss_armor) do
        total_cost
      else
        nil
      end
    end
    |> Enum.reject(&is_nil/1)
    |> Enum.min()
  end

  defp combinations([], _), do: [[]]
  defp combinations(list, n) when n <= 0, do: [[]]
  defp combinations([head | tail], n) do
    with_head = for rest <- combinations(tail, n - 1), do: [head | rest]
    without_head = combinations(tail, n)
    with_head ++ without_head
  end

  defp player_wins?(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor) do
    player_damage = max(1, player_damage - boss_armor)
    boss_damage = max(1, boss_damage - player_armor)

    player_turns = div(boss_hp + player_damage - 1, player_damage)
    boss_turns = div(player_hp + boss_damage - 1, boss_damage)

    player_turns <= boss_turns
  end
end

RPGSimulator.run()
