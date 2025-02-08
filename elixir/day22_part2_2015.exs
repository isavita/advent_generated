
defmodule Solver do
  def run(state, min_mana, player_turn) when state.mana_spent >= min_mana, do: min_mana
  def run(state, min_mana, _player_turn) when state.boss_hp <= 0, do: min(min_mana, state.mana_spent)
  def run(state, min_mana, _player_turn) when state.player_hp <= 0, do: min_mana

  def run(state, min_mana, true) do
    state = %{state | player_hp: state.player_hp - 1}

    if state.player_hp <= 0 do
      min_mana
    else
      run_turn(state, min_mana, true)
    end
  end

  def run(state, min_mana, false) do
    run_turn(state, min_mana, false)
  end

  defp run_turn(state, min_mana, player_turn) do
    state = apply_effects(state)

    if not player_turn do
      damage = if state.shield_timer > 0, do: max(1, state.boss_damage - 7), else: state.boss_damage
      new_state = %{state | player_hp: state.player_hp - damage}
      run(new_state, min_mana, true)
    else
      spells = [
        {:magic_missile, 53, &magic_missile/1},
        {:drain, 73, &drain/1},
        {:shield, 113, &shield/1},
        {:poison, 173, &poison/1},
        {:recharge, 229, &recharge/1}
      ]

      Enum.reduce(spells, min_mana, fn {spell, cost, fun}, acc ->
        if can_cast?(state, spell, cost) do
          new_state =
            state
            |> cast(cost)
            |> fun.()

          run(new_state, acc, false)
        else
          acc
        end
      end)
    end
  end

  defp apply_effects(state) do
    state
    |> apply_shield()
    |> apply_poison()
    |> apply_recharge()
  end

  defp apply_shield(state) do
    if state.shield_timer > 0, do: %{state | shield_timer: state.shield_timer - 1}, else: state
  end

  defp apply_poison(state) do
    if state.poison_timer > 0,
      do: %{state | boss_hp: state.boss_hp - 3, poison_timer: state.poison_timer - 1},
      else: state
  end

  defp apply_recharge(state) do
    if state.recharge_timer > 0,
      do: %{state | player_mana: state.player_mana + 101, recharge_timer: state.recharge_timer - 1},
      else: state
  end

  defp can_cast?(state, spell, cost) do
    case spell do
      :shield -> state.player_mana >= cost and state.shield_timer == 0
      :poison -> state.player_mana >= cost and state.poison_timer == 0
      :recharge -> state.player_mana >= cost and state.recharge_timer == 0
      _ -> state.player_mana >= cost
    end
  end

  defp cast(state, cost) do
    %{state | player_mana: state.player_mana - cost, mana_spent: state.mana_spent + cost}
  end

  defp magic_missile(state) do
    %{state | boss_hp: state.boss_hp - 4}
  end

  defp drain(state) do
    %{state | boss_hp: state.boss_hp - 2, player_hp: state.player_hp + 2}
  end

  defp shield(state) do
    %{state | shield_timer: 6}
  end

  defp poison(state) do
    %{state | poison_timer: 6}
  end

  defp recharge(state) do
    %{state | recharge_timer: 5}
  end
end

[boss_hp, boss_damage] =
  File.read!("input.txt")
  |> String.split("\n", trim: true)
  |> Enum.map(&String.split(&1, ": ", trim: true))
  |> Enum.map(fn [_, v] -> String.to_integer(v) end)

initial_state = %{
  player_hp: 50,
  player_mana: 500,
  boss_hp: boss_hp,
  boss_damage: boss_damage,
  shield_timer: 0,
  poison_timer: 0,
  recharge_timer: 0,
  mana_spent: 0
}

Solver.run(initial_state, :infinity, true)
|> IO.inspect()
