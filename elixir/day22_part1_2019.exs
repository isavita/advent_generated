
defmodule Deck do
  @size 10_007

  def main do
    deck = Enum.to_list(0..(@size - 1))
    input = File.read!("input.txt") |> String.split("\n", trim: true)

    final_deck = Enum.reduce(input, deck, &process_line/2)
    IO.puts(find_2019(final_deck))
  end

  defp process_line("deal into new stack", deck), do: Enum.reverse(deck)
  
  defp process_line("cut " <> n_str, deck) do
    n = String.to_integer(n_str)
    cut_n(deck, n)
  end

  defp process_line("deal with increment " <> n_str, deck) do
    n = String.to_integer(n_str)
    deal_with_increment(deck, n)
  end

  defp cut_n(deck, n) when n >= 0, do: Enum.concat(Enum.drop(deck, n), Enum.take(deck, n))
  defp cut_n(deck, n), do: Enum.concat(Enum.drop(deck, length(deck) + n), Enum.take(deck, length(deck) + n))

  defp deal_with_increment(deck, n) do
    new_deck = List.duplicate(0, @size)
    Enum.reduce(0..(@size - 1), new_deck, fn i, acc ->
      List.replace_at(acc, rem(i * n, @size), Enum.at(deck, i))
    end)
  end

  defp find_2019(deck) do
    Enum.find_index(deck, &(&1 == 2019))
  end
end

Deck.main()
