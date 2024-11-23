
defmodule CardGame do
  def solve do
    {:ok, content} = File.read("input.txt")
    [player1, player2] = content 
    |> String.split("\n\n")
    |> Enum.map(fn deck -> 
      deck 
      |> String.split("\n")
      |> Enum.drop(1)
      |> Enum.map(&String.to_integer/1)
    end)

    play_game(player1, player2)
    |> calculate_score()
    |> IO.puts()
  end

  defp play_game(deck1, deck2) do
    case {deck1, deck2} do
      {[], deck2} -> deck2
      {deck1, []} -> deck1
      {[card1 | rest1], [card2 | rest2]} ->
        cond do
          card1 > card2 -> 
            play_game(rest1 ++ [card1, card2], rest2)
          true -> 
            play_game(rest1, rest2 ++ [card2, card1])
        end
    end
  end

  defp calculate_score(deck) do
    deck
    |> Enum.reverse()
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {card, multiplier}, acc -> 
      acc + card * multiplier 
    end)
  end
end

CardGame.solve()
