defmodule Day2 do
  def call() do
    input = File.read!("input.txt")
    result = calculate_divisible_values_sum(input)
    IO.puts(result)
  end

  defp calculate_divisible_values_sum(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_row/1)
    |> Enum.map(&find_divisible_pair_result/1)
    |> Enum.sum()
  end

  defp parse_row(row) do
    row
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  defp find_divisible_pair_result(row) do
    row
    |> Enum.flat_map(fn x ->
      Enum.filter(row, fn y -> x != y && rem(max(x, y), min(x, y)) == 0 end)
      |> Enum.map(fn y -> div(max(x, y), min(x, y)) end)
    end)
    |> Enum.at(0)
  end
end

Day2.call()
