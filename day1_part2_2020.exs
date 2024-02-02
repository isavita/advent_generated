
defmodule ReportRepair do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
    {a, b} = find_two_entries(input, 2020)
    result = a * b
    {c, d, e} = find_three_entries(input, 2020)
    result2 = c * d * e
    {result, result2}
  end

  defp find_two_entries(input, target) do
    Enum.reduce(input, nil, fn x, acc ->
      case Enum.find(input, fn y -> x + y == target end) do
        nil -> acc
        y -> {x, y}
      end
    end)
  end

  defp find_three_entries(input, target) do
    Enum.reduce(input, nil, fn x, acc ->
      case Enum.find(input, fn y -> Enum.find(input, fn z -> x + y + z == target end) end) do
        nil -> acc
        y -> {x, y, Enum.find(input, fn z -> x + y + z == target end)}
      end
    end)
  end
end
