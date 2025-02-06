
defmodule Card do
  defstruct winnings: MapSet.new(), givens: MapSet.new(), total_count: 1
end

defmodule Solver do
  def get_points_for_card(card) do
    MapSet.size(MapSet.intersection(card.winnings, card.givens))
  end

  def lex_line_into_card(line) do
    [_, card_data_str] = Regex.split(~r/: /, line)
    [winning_str, given_str] = String.split(card_data_str, " | ")

    winnings =
      Regex.scan(~r/[0-9]{1,2}/, winning_str)
      |> List.flatten()
      |> MapSet.new()

    givens =
      Regex.scan(~r/[0-9]{1,2}/, given_str)
      |> List.flatten()
      |> MapSet.new()

    %Card{winnings: winnings, givens: givens}
  end

  def solve(cards, acc \\ 0)
  def solve([], acc), do: acc

  def solve([card | rest_cards], acc) do
    points = get_points_for_card(card)

    updated_rest_cards =
      Enum.take(rest_cards, points)
      |> Enum.map(fn c -> %{c | total_count: c.total_count + card.total_count} end)

    remaining_cards =
      Enum.drop(rest_cards, points)
      |> Enum.concat(updated_rest_cards)
      |> Enum.sort_by(&(&1.total_count), :desc)

    solve(remaining_cards, acc + card.total_count)
  end
end

{:ok, file} = File.read("input.txt")

cards =
  file
  |> String.trim()
  |> String.split("\n")
  |> Enum.map(&Solver.lex_line_into_card/1)

Solver.solve(cards)
|> IO.puts()
