
defmodule AlchemicalReduction do
  def call do
    polymer = File.read!("input.txt") |> String.trim()

    fully_react(polymer) |> String.length()
  end

  defp fully_react(polymer) do
    reacted = react(polymer)

    if reacted == polymer do
      reacted
    else
      fully_react(reacted)
    end
  end

  defp react(polymer) do
    reacted = Enum.reduce(String.graphemes(polymer), [], fn unit, acc ->
      case acc do
        [] -> [unit]
        [last | rest] ->
          if opposite_polarity?(last, unit) do
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
end
