
defmodule RPGSimulator do
  @weapons [
    %{cost: 8, damage: 4, armor: 0},
    %{cost: 10, damage: 5, armor: 0},
    %{cost: 25, damage: 6, armor: 0},
    %{cost: 40, damage: 7, armor: 0},
    %{cost: 74, damage: 8, armor: 0}
  ]

  @armor [
    %{cost: 0, damage: 0, armor: 0},
    %{cost: 13, damage: 0, armor: 1},
    %{cost: 31, damage: 0, armor: 2},
    %{cost: 53, damage: 0, armor: 3},
    %{cost: 75, damage: 0, armor: 4},
    %{cost: 102, damage: 0, armor: 5}
  ]

  @rings [
    %{cost: 0, damage: 0, armor: 0},
    %{cost: 25, damage: 1, armor: 0},
    %{cost: 50, damage: 2, armor: 0},
    %{cost: 100, damage: 3, armor: 0},
    %{cost: 20, damage: 0, armor: 1},
    %{cost: 40, damage: 0, armor: 2},
    %{cost: 80, damage: 0, armor: 3}
  ]

  def read_boss_stats(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      Regex.run(~r/(\w+): (\d+)/, line)
      |> List.last()
      |> String.to_integer()
    end)
    |> then(fn [hit_points, damage, armor] ->
      %{hit_points: hit_points, damage: damage, armor: armor}
    end)
  end

  def simulate_fight(player_damage, player_armor, boss_stats) do
    player_hit_points = 100
    boss_hit_points = boss_stats.hit_points
    boss_damage = boss_stats.damage
    boss_armor = boss_stats.armor

    simulate_turn(
      player_hit_points,
      player_damage,
      player_armor,
      boss_hit_points,
      boss_damage,
      boss_armor,
      :player
    )
  end

  defp simulate_turn(
         player_hit_points,
         player_damage,
         player_armor,
         boss_hit_points,
         boss_damage,
         boss_armor,
         turn
       ) do
    cond do
      player_hit_points <= 0 -> :boss_wins
      boss_hit_points <= 0 -> :player_wins
      turn == :player ->
        damage_to_boss = max(1, player_damage - boss_armor)
        simulate_turn(
          player_hit_points,
          player_damage,
          player_armor,
          boss_hit_points - damage_to_boss,
          boss_damage,
          boss_armor,
          :boss
        )
      turn == :boss ->
        damage_to_player = max(1, boss_damage - player_armor)
        simulate_turn(
          player_hit_points - damage_to_player,
          player_damage,
          player_armor,
          boss_hit_points,
          boss_damage,
          boss_armor,
          :player
        )
    end
  end

  def find_min_winning_cost(boss_stats) do
    for weapon <- @weapons,
        armor <- @armor,
        ring1 <- @rings,
        ring2 <- @rings,
        ring1 != ring2 do
      player_damage = weapon.damage + armor.damage + ring1.damage + ring2.damage
      player_armor = weapon.armor + armor.armor + ring1.armor + ring2.armor
      cost = weapon.cost + armor.cost + ring1.cost + ring2.cost

      if simulate_fight(player_damage, player_armor, boss_stats) == :player_wins do
        {cost, player_damage, player_armor}
      else
        nil
      end
    end
    |> Enum.filter(& &1)
    |> Enum.min_by(fn {cost, _, _} -> cost end)
    |> case do
      nil -> nil
      {cost, _, _} -> cost
    end
  end

  def find_max_losing_cost(boss_stats) do
    for weapon <- @weapons,
        armor <- @armor,
        ring1 <- @rings,
        ring2 <- @rings,
        ring1 != ring2 do
      player_damage = weapon.damage + armor.damage + ring1.damage + ring2.damage
      player_armor = weapon.armor + armor.armor + ring1.armor + ring2.armor
      cost = weapon.cost + armor.cost + ring1.cost + ring2.cost

      if simulate_fight(player_damage, player_armor, boss_stats) == :boss_wins do
        {cost, player_damage, player_armor}
      else
        nil
      end
    end
    |> Enum.filter(& &1)
    |> Enum.max_by(fn {cost, _, _} -> cost end)
    |> case do
      nil -> nil
      {cost, _, _} -> cost
    end
  end

  def main(filename) do
    boss_stats = read_boss_stats(filename)
    min_cost = find_min_winning_cost(boss_stats)
    max_cost = find_max_losing_cost(boss_stats)

    IO.puts("Minimum cost to win: #{min_cost}")
    IO.puts("Maximum cost to lose: #{max_cost}")
  end
end

RPGSimulator.main("input.txt")
