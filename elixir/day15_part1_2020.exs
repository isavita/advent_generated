defmodule Day15 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp play_game(starting_numbers, final_turn) do
    starting_turns =
      Enum.with_index(starting_numbers, 1)
      |> Enum.into(%{}, fn {num, index} -> {num, [index]} end)

    {_, last_spoken} =
      Enum.reduce(1..final_turn, {starting_turns, Enum.at(starting_numbers, -1)}, fn
        turn, {spoken_turns, last_num} when turn <= length(starting_numbers) ->
          {spoken_turns, last_num}

        turn, {spoken_turns, last_num} ->
          next_num =
            case Map.get(spoken_turns, last_num) do
              [prev_turn, prev_prev_turn] -> prev_turn - prev_prev_turn
              [_prev_turn] -> 0
              _ -> raise "Unexpected pattern"
            end

          new_turns =
            Map.update(spoken_turns, next_num, [turn], fn turns ->
              # Keep only the last two occurrences
              [turn | turns] |> Enum.take(2)
            end)

          {new_turns, next_num}
      end)

    last_spoken
  end

  def call do
    starting_numbers = read_input()
    result = play_game(starting_numbers, 2020)
    IO.puts("The 2020th number spoken will be: #{result}")
  end
end

Day15.call()
