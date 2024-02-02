
defmodule Day6 do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)

    message_length = String.length(Enum.at(input, 0))
    message = Enum.reduce(0..(message_length - 1), "", fn i, acc ->
      column = Enum.map(input, &String.at(&1, i))
      most_frequent_char = Enum.reduce(column, %{}, fn char, counts ->
        Map.update(counts, char, 1, &(&1 + 1))
      end) |> Map.to_list() |> Enum.max_by(&elem(&1, 1)) |> elem(0)

      acc <> most_frequent_char
    end)

    message
  end
end
