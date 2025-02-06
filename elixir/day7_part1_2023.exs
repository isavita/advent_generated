
defmodule CamelCards do
  def hand_type(hand) do
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
      _ -> :error
    end
  end

  def card_value(card) do
    case card do
      "A" -> 14
      "K" -> 13
      "Q" -> 12
      "J" -> 11
      "T" -> 10
      _ -> String.to_integer(card)
    end
  end

  def hand_compare(hand1, hand2) do
    type1 = hand_type(hand1)
    type2 = hand_type(hand2)

    type_order = [
      :high_card,
      :one_pair,
      :two_pair,
      :three_of_a_kind,
      :full_house,
      :four_of_a_kind,
      :five_of_a_kind
    ]

    if type1 == type2 do
      cards1 = String.graphemes(hand1) |> Enum.map(&card_value/1)
      cards2 = String.graphemes(hand2) |> Enum.map(&card_value/1)

      compare_cards(cards1, cards2)
    else
      Enum.find_index(type_order, &(&1 == type1)) <
        Enum.find_index(type_order, &(&1 == type2))
    end
  end

  def compare_cards([], []), do: false

  def compare_cards([h1 | t1], [h2 | t2]) do
    if h1 == h2 do
      compare_cards(t1, t2)
    else
      h1 < h2
    end
  end

  def solve(input) do
    hands =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        [hand, bid] = String.split(line)
        {hand, String.to_integer(bid)}
      end)

    sorted_hands =
      Enum.sort_by(hands, fn {hand, _bid} ->
        {hand_type(hand)}  # Sort by hand_type to group them correctly
         |> Tuple.append(String.graphemes(hand) |> Enum.map(&card_value/1))
      end, fn {type1, cards1}, {type2, cards2} ->
        type_order = [
          :high_card,
          :one_pair,
          :two_pair,
          :three_of_a_kind,
          :full_house,
          :four_of_a_kind,
          :five_of_a_kind
        ]
      
        if type1 == type2 do
          compare_cards(cards1, cards2)
        else
            Enum.find_index(type_order, &(&1 == type1)) <
              Enum.find_index(type_order, &(&1 == type2))
        end

      end)
    
      sorted_hands
      |> Enum.with_index(1)
      |> Enum.map(fn {{_hand, bid}, rank} -> bid * rank end)
      |> Enum.sum()
  end
end

input = File.read!("input.txt")
result = CamelCards.solve(input)
IO.puts(result)
