
defmodule AdapterArray do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
    sorted_adapters = [0 | Enum.sort(input) ++ [Enum.max(input) + 3]]
    {ones, threes} = Enum.reduce(Enum.chunk_every(sorted_adapters, 2, 1, :discard), {0, 0}, fn [a, b], {ones, threes} ->
      case b - a do
        1 -> {ones + 1, threes}
        3 -> {ones, threes + 1}
      end
    end)
    ones * threes
  end
end
