
defmodule Santa do
  def call do
    input = File.read!("input.txt") |> String.trim
    floor = input |> String.graphemes() |> Enum.reduce(0, fn char, acc ->
      if char == "(", do: acc + 1, else: acc - 1
    end)

    basement_position = input |> String.graphemes() |> Enum.with_index() |> Enum.reduce({0, 0}, fn {char, index}, {floor, position} ->
      if floor == -1, do: {floor, position}, else: {floor + (if char == "(", do: 1, else: -1), position + 1}
    end) |> elem(1)

    {floor, basement_position}
  end
end
