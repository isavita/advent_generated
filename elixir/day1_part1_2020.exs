
defmodule ReportRepair do
  def call do
    input = File.read!("input.txt") |> String.split("\n") |> Enum.map(&String.to_integer/1)
    result = find_entries(input, 2020)
    elem(result, 0) * elem(result, 1)
  end

  defp find_entries(input, target) do
    Enum.reduce(input, nil, fn x, acc ->
      case Enum.find(input, fn y -> x + y == target end) do
        nil -> acc
        y -> {x, y}
      end
    end)
  end
end
