
defmodule Day2 do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    {twos, threes} = Enum.reduce(input, {0, 0}, fn id, {twos, threes} ->
      counts = id |> String.graphemes() |> Enum.frequencies()
      twos_in_id = counts |> Enum.any?(fn {_char, count} -> count == 2 end)
      threes_in_id = counts |> Enum.any?(fn {_char, count} -> count == 3 end)
      {twos + (twos_in_id && 1 || 0), threes + (threes_in_id && 1 || 0)}
    end)
    twos * threes
  end
end
