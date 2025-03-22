
defmodule CamelCards do
  @card_values %{
    "A" => 14,
    "K" => 13,
    "Q" => 12,
    "J" => 11,
    "T" => 10,
    "9" => 9,
    "8" => 8,
    "7" => 7,
    "6" => 6,
    "5" => 5,
    "4" => 4,
    "3" => 3,
    "2" => 2
  }

  @joker_card_values Map.put(@card_values, "J", 1)

  defp hand_type(hand) do
    counts =
      hand
      |> String.graphemes()
      |> Enum.frequencies()
      |> Map.values()
      |> Enum.sort(:desc)

    case counts do
      [5] -> :five_of_a_kind
      [4, 1] -> :four_of_a_kind
      [3, 2] -> :full_house
      [3, 1, 1] -> :three_of_a_kind
      [2, 2, 1] -> :two_pair
      [2, 1, 1, 1] -> :one_pair
      [1, 1, 1, 1, 1] -> :high_card
    end
  end

  defp hand_type_with_joker(hand) do
    cards = String.graphemes(hand)
    joker_count = Enum.count(cards, &(&1 == "J"))

    if joker_count == 0 do
      hand_type(hand)
    else
      non_joker_cards = Enum.reject(cards, &(&1 == "J"))

      if Enum.empty?(non_joker_cards) do
        :five_of_a_kind
      else
        {most_frequent_card, _} =
          Enum.frequencies(non_joker_cards)
          |> Enum.max_by(fn {_, count} -> count end)

        new_hand = String.replace(hand, "J", most_frequent_card)
        hand_type(new_hand)
      end
    end
  end

  defp compare_hands(hand1, hand2, card_values) do
    hand1_cards = String.graphemes(hand1)
    hand2_cards = String.graphemes(hand2)

    Enum.zip(hand1_cards, hand2_cards)
    |> Enum.reduce_while(:eq, fn {card1, card2}, acc ->
      if acc != :eq do
        {:halt, acc}
      else
        val1 = Map.get(card_values, card1)
        val2 = Map.get(card_values, card2)

        cond do
          val1 > val2 -> {:halt, :gt}
          val1 < val2 -> {:halt, :lt}
          true -> {:cont, :eq}
        end
      end
    end)
  end

  def solve(filename, joker_rule \\ false) do
    card_values = if joker_rule, do: @joker_card_values, else: @card_values

    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [hand, bid] = String.split(line, " ")
      {hand, String.to_integer(bid)}
    end)
    |> Enum.sort_by(fn {hand, _} ->
      type = if joker_rule, do: hand_type_with_joker(hand), else: hand_type(hand)
      {type, hand}
    end, fn {type1, hand1}, {type2, hand2} ->
      cond do
        type1 != type2 ->
          type_rank(type1) <= type_rank(type2)

        true ->
          case compare_hands(hand1, hand2, card_values) do
            :gt -> false
            :lt -> true
            :eq -> false
          end
      end
    end)
    |> Enum.with_index(1)
    |> Enum.map(fn {{_, bid}, rank} -> bid * rank end)
    |> Enum.sum()
  end

  defp type_rank(type) do
    case type do
      :five_of_a_kind -> 7
      :four_of_a_kind -> 6
      :full_house -> 5
      :three_of_a_kind -> 4
      :two_pair -> 3
      :one_pair -> 2
      :high_card -> 1
    end
  end
end

# Main execution
filename = "input.txt"
part1_result = CamelCards.solve(filename)
part2_result = CamelCards.solve(filename, true)

IO.puts("Part 1: #{part1_result}")
IO.puts("Part 2: #{part2_result}")
