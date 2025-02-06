
defmodule Scratchcard do
  def calculate_points(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_card/1)
    |> Enum.map(&calculate_card_points/1)
    |> Enum.sum()
  end

  defp parse_card(line) do
    [_, numbers] = String.split(line, ": ", parts: 2)
    [winning_numbers, my_numbers] = String.split(numbers, " | ", parts: 2)

    {parse_numbers(winning_numbers), parse_numbers(my_numbers)}
  end

  defp parse_numbers(numbers_str) do
    numbers_str
    |> String.split(" ", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> MapSet.new()
  end
  
  defp calculate_card_points({winning_numbers, my_numbers}) do
    matches = MapSet.intersection(winning_numbers, my_numbers) |> MapSet.size()
    if matches > 0 do
      :math.pow(2, matches - 1) |> trunc()
    else
      0
    end
  end
end

IO.puts(Scratchcard.calculate_points("input.txt"))
