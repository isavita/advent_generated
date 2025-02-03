
defmodule Advent do
  def read_input do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      String.split(line, ": ", parts: 2)
      |> List.last()
      |> String.to_integer()
    end)
    |> then(fn [boss_hp, boss_damage] -> {boss_hp, boss_damage} end)
  end

  def min_mana_to_win({boss_hp, boss_damage}) do
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

    simulate(initial_state, true, 1_000_000)
  end

  defp simulate(state, player_turn, min_mana) do
    if state.mana_spent >= min_mana do
      min_mana
    else
      cond do
        state.boss_hp <= 0 ->
          min_mana = min(min_mana, state.mana_spent)
          min_mana
        state.player_hp <= 0 ->
          min_mana
        true ->
          state = apply_effects(state)
          if player_turn do
            simulate_player_turn(state, min_mana)
          else
            simulate_boss_turn(state, min_mana)
          end
      end
    end
  end

  defp apply_effects(state) do
    state = %{state | shield_timer: max(state.shield_timer - 1, 0)}

    state =
      if state.poison_timer > 0 do
        %{state | boss_hp: state.boss_hp - 3, poison_timer: state.poison_timer - 1}
      else
        state
      end

    if state.recharge_timer > 0 do
      %{state | player_mana: state.player_mana + 101, recharge_timer: state.recharge_timer - 1}
    else
      state
    end
  end

  defp simulate_boss_turn(state, min_mana) do
      damage =
        if state.shield_timer > 0 do
            max(state.boss_damage - 7,1)
        else
            state.boss_damage
        end
    
    new_state = %{state | player_hp: state.player_hp - damage}
    simulate(new_state, true, min_mana)
  end

  defp simulate_player_turn(state, min_mana) do
    Enum.reduce(possible_spells(state), min_mana, fn spell, acc ->
        if state.player_mana >= spell.cost do
            new_state = %{state | player_mana: state.player_mana - spell.cost, mana_spent: state.mana_spent + spell.cost}
            simulate(spell.apply.(new_state), false, acc)
        else
            acc
        end
    end)
  end


  defp possible_spells(state) do
        [
        %{cost: 53, apply: fn state -> %{state | boss_hp: state.boss_hp - 4} end },
        %{cost: 73, apply: fn state -> %{state | boss_hp: state.boss_hp - 2, player_hp: state.player_hp + 2} end},
        %{cost: 113, apply: fn state -> if state.shield_timer == 0, do: %{state | shield_timer: 6}, else: state end},
        %{cost: 173, apply: fn state -> if state.poison_timer == 0, do: %{state | poison_timer: 6}, else: state end},
        %{cost: 229, apply: fn state -> if state.recharge_timer == 0, do: %{state | recharge_timer: 5}, else: state end}
      ]
  end
end


{boss_hp, boss_damage} = Advent.read_input()
result = Advent.min_mana_to_win({boss_hp, boss_damage})
IO.puts(result)
