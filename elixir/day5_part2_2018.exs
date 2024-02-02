
defmodule AlchemicalReduction do
  def call do
    polymer = File.read!("input.txt") |> String.trim()

    fully_reacted_polymer = react_polymer(polymer)
    shortest_polymer_length = shortest_polymer_after_removal(fully_reacted_polymer)

    shortest_polymer_length
  end

  defp react_polymer(polymer) do
    reacted = Enum.reduce(String.graphemes(polymer), [], fn unit, acc ->
      case acc do
        [] -> [unit]
        [last_unit | rest] ->
          if opposite_polarity?(unit, last_unit) do
            rest
          else
            [unit | acc]
          end
      end
    end)

    reacted |> Enum.reverse() |> Enum.join()
  end

  defp opposite_polarity?(unit1, unit2) do
    String.downcase(unit1) == String.downcase(unit2) && unit1 != unit2
  end

  defp shortest_polymer_after_removal(polymer) do
    unique_units = polymer |> String.downcase() |> String.graphemes() |> Enum.uniq()

    shortest_polymer = Enum.map(unique_units, fn unit ->
      polymer
      |> String.replace(~r/#{unit}/i, "")
      |> react_polymer()
      |> String.length()
    end)
    |> Enum.min()

    shortest_polymer
  end
end
