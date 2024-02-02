
defmodule Lanternfish do
  def call do
    {part1, part2} = File.read!("input.txt")
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce(new_counter(), fn age, acc -> Map.update!(acc, age, &(&1 + 1)) end)
    |> simulate(256)

    {simulate(part1, 80 - 1), part2}
  end

  defp new_counter, do: Enum.into(0..8, %{}, fn x -> {x, 0} end)

  defp simulate(counter, 0), do: Enum.reduce(counter, 0, fn {_k, v}, acc -> acc + v end)

  defp simulate(counter, days) do
    new_counter = %{
      0 => Map.get(counter, 1),
      1 => Map.get(counter, 2),
      2 => Map.get(counter, 3),
      3 => Map.get(counter, 4),
      4 => Map.get(counter, 5),
      5 => Map.get(counter, 6),
      6 => Map.get(counter, 7) + Map.get(counter, 0),
      7 => Map.get(counter, 8),
      8 => Map.get(counter, 0)
    }
    simulate(new_counter, days - 1)
  end
end
