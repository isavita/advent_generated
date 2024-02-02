
defmodule AuntSue do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> Enum.find_index(&match?(&1))
    |> Kernel.+(1)
  end

  defp parse_line(line) do
    [_, rest] = String.split(line, ": ", parts: 2)
    Enum.map(String.split(rest, ", "), fn item ->
      [key, value] = String.split(item, ": ")
      {String.to_atom(key), String.to_integer(value)}
    end)
    |> Enum.into(%{})
  end

  defp match?(sue) do
    sue
    |> Enum.all?(fn
      {:children, 3} -> true
      {:cats, x} when x > 7 -> true
      {:samoyeds, 2} -> true
      {:pomeranians, x} when x < 3 -> true
      {:akitas, 0} -> true
      {:vizslas, 0} -> true
      {:goldfish, x} when x < 5 -> true
      {:trees, x} when x > 3 -> true
      {:cars, 2} -> true
      {:perfumes, 1} -> true
      _ -> false
    end)
  end
end
