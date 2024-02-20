defmodule Day13 do
  def call() do
    input = File.read!("input.txt")
    {names, happiness_map} = parse_input(input)
    # Add ourselves to the list, no need to modify the happiness_map as all interactions with us are neutral (0).
    names_with_self = ["You" | names]
    optimal_happiness = names_with_self
                        |> permutations()
                        |> Enum.map(&calculate_happiness(&1, happiness_map)) # No change needed here as our impact is neutral.
                        |> Enum.max()
    IO.puts(optimal_happiness)
  end

  defp permutations(list), do: permute(list, [])

  defp permute([], acc), do: [Enum.reverse(acc)]
  defp permute(list, acc) do
    Enum.flat_map(list, fn el ->
      permute(list -- [el], [el | acc])
    end)
  end

  defp parse_input(input) do
    lines = String.split(input, "\n", trim: true)

    {names, happiness_map} = Enum.reduce(lines, {[], %{}}, fn line, {names_acc, acc} ->
      case String.split(line, " ") do
        [person1, "would", action, amount, "happiness", "units", "by", "sitting", "next", "to", person2_dot] ->
          units = parse_units(action, String.to_integer(amount))
          person2 = String.trim_trailing(person2_dot, ".")
          names = Enum.uniq([person1, person2 | names_acc])
          happiness_map = Map.update(acc, {person1, person2}, units, &(&1 + units))
          {names, happiness_map}

        _ ->
          {names_acc, acc}
      end
    end)

    {names, happiness_map}
  end

  defp parse_units("gain", amount), do: amount
  defp parse_units("lose", amount), do: -amount

  defp calculate_happiness(arrangement, happiness_map) do
    pairs = Enum.zip(arrangement, Enum.drop(arrangement, 1) ++ [List.first(arrangement)])
    Enum.map(pairs, fn {a, b} ->
      Map.get(happiness_map, {a, b}, 0) + Map.get(happiness_map, {b, a}, 0)
    end)
    |> Enum.sum()
  end
end

Day13.call()
