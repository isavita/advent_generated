
defmodule Day23 do
  def main do
    input_string = File.read!("input.txt") |> String.trim()
    input_cups = input_string |> String.graphemes() |> Enum.map(&String.to_integer/1)

    total_cups = 1_000_000
    total_moves = 10_000_000

    cups = build_cups_map(input_cups, total_cups)

    current_cup = hd(input_cups)

    final_cups = simulate(current_cup, cups, total_moves, total_cups)

    cup_after_1 = Map.get(final_cups, 1)
    cup_after_that = Map.get(final_cups, cup_after_1)
    result = cup_after_1 * cup_after_that

    IO.puts(result)
  end

  defp build_cups_map(input_cups, total_cups) do
    num_input_cups = length(input_cups)
    first_input_cup = hd(input_cups)
    last_input_cup = List.last(input_cups)

    input_map = input_cups
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.into(%{}, fn [a, b] -> {a, b} end)

    map_with_input_tail = Map.put(input_map, last_input_cup, num_input_cups + 1)

    sequential_map = Enum.reduce(num_input_cups + 1 .. total_cups - 1, map_with_input_tail, fn cup, acc ->
      Map.put(acc, cup, cup + 1)
    end)

    Map.put(sequential_map, total_cups, first_input_cup)
  end

  defp simulate(_current_cup, cups, 0, _total_cups), do: cups

  defp simulate(current_cup, cups, remaining_moves, total_cups) do
    pickup1 = Map.get(cups, current_cup)
    pickup2 = Map.get(cups, pickup1)
    pickup3 = Map.get(cups, pickup2)
    picked_up = [pickup1, pickup2, pickup3]

    next_cup_after_current = Map.get(cups, pickup3)
    cups_after_removal = Map.put(cups, current_cup, next_cup_after_current)

    destination_cup = find_destination(current_cup - 1, picked_up, total_cups)

    next_cup_after_destination = Map.get(cups_after_removal, destination_cup)
    cups_after_insertion1 = Map.put(cups_after_removal, destination_cup, pickup1)
    cups_after_insertion2 = Map.put(cups_after_insertion1, pickup3, next_cup_after_destination)

    next_current_cup = next_cup_after_current

    simulate(next_current_cup, cups_after_insertion2, remaining_moves - 1, total_cups)
  end

  defp find_destination(destination, picked_up, total_cups) do
    cond do
      destination == 0 ->
        find_destination(total_cups, picked_up, total_cups)
      Enum.member?(picked_up, destination) ->
        find_destination(destination - 1, picked_up, total_cups)
      true ->
        destination
    end
  end
end

Day23.main()
