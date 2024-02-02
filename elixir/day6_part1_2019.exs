
defmodule OrbitMap do
  def call do
    input = File.read!("input.txt")
    orbits = input |> String.split("\n") |> parse_orbits()

    total_orbits(orbits)
  end

  defp parse_orbits(input) do
    input
    |> Enum.map(&String.split(&1, ")"))
    |> Enum.reduce(%{}, fn [center, orbit], acc ->
      Map.update(acc, orbit, [center], &(&1 ++ [center]))
    end)
  end

  defp total_orbits(orbits) do
    orbits
    |> Map.keys()
    |> Enum.reduce(0, fn orbit, acc ->
      acc + count_orbits(orbits, orbit)
    end)
  end

  defp count_orbits(orbits, orbit) do
    case orbits[orbit] do
      nil -> 0
      centers -> Enum.reduce(centers, 0, fn center, acc ->
        acc + 1 + count_orbits(orbits, center)
      end)
    end
  end
end
