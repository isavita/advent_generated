
defmodule CubeConundrum do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.filter(&String.contains?(&1, "Game"))
    |> Enum.map(&parse_game/1)
    |> Enum.filter(&possible_game?(&1, %{red: 12, green: 13, blue: 14}))
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  defp parse_game(line) do
    [id_part | data] = String.split(line, ": ")
    id = String.slice(id_part, 5..-1) |> String.to_integer()
    subsets = String.split(Enum.at(data, 0), "; ", trim: true)
    {id, subsets}
  end

  defp possible_game?({id, subsets}, bag) do
    subsets
    |> Enum.map(&parse_subset/1)
    |> Enum.all?(&subset_possible?(&1, bag))
  end

  defp parse_subset(subset) do
    subset
    |> String.split(~r/,?\s+/, trim: true)
    |> Enum.chunk_every(2)
    |> Enum.map(fn [count, color] -> {String.to_atom(color), String.to_integer(count)} end)
    |> Enum.into(%{})
  end

  defp subset_possible?(subset, bag) do
    Enum.all?(subset, fn {color, count} -> Map.get(bag, color, 0) >= count end)
  end
end
