defmodule RecursiveCombat do
  def main do
    {player1, player2} = read_input("input.txt")
    {final_player1, final_player2} = play_recursive_combat(player1, player2)
    winning_deck = if Enum.count(final_player1) > 0, do: final_player1, else: final_player2
    IO.puts score(winning_deck)
  end

  defp read_input(file_path) do
    file_path
    |> File.read!()
    |> String.split("\n\n")
    |> Enum.map(fn section ->
      section
      |> String.split("\n")
      |> Enum.drop(1)
      |> Enum.map(&String.to_integer/1)
    end)
    |> List.to_tuple()
  end

  defp play_recursive_combat(player1, player2, previous_rounds \\ %{})
  defp play_recursive_combat([], player2, _), do: {[], player2}
  defp play_recursive_combat(player1, [], _), do: {player1, []}
  defp play_recursive_combat(player1 = [card1 | rest1], player2 = [card2 | rest2], previous_rounds) do
    round_key = {player1, player2}
    if Map.has_key?(previous_rounds, round_key) do
      {player1, []}
    else
      previous_rounds = Map.put(previous_rounds, round_key, true)

      {new_player1, new_player2} =
        if Enum.count(player1) - 1 >= card1 and Enum.count(player2) - 1 >= card2 do
          sub_player1 = Enum.take(rest1, card1)
          sub_player2 = Enum.take(rest2, card2)
          {result_player1, _result_player2} = play_recursive_combat(sub_player1, sub_player2)
          if Enum.count(result_player1) > 0, do: {rest1 ++ [card1, card2], rest2}, else: {rest1, rest2 ++ [card2, card1]}
        else
          if card1 > card2, do: {rest1 ++ [card1, card2], rest2}, else: {rest1, rest2 ++ [card2, card1]}
        end

      play_recursive_combat(new_player1, new_player2, previous_rounds)
    end
  end

  defp score(deck) do
    deck
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {card, index}, acc -> acc + card * (Enum.count(deck) - index + 1) end)
  end
end

RecursiveCombat.main()