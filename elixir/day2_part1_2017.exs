defmodule Day2 do
  def call() do
    input = File.read!("input.txt")
    checksum = calculate_checksum(input)
    IO.puts(checksum)
  end

  defp calculate_checksum(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_row/1)
    |> Enum.map(&row_difference/1)
    |> Enum.sum()
  end

  defp parse_row(row) do
    row
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  defp row_difference(row) do
    max = Enum.max(row)
    min = Enum.min(row)
    max - min
  end
end

Day2.call()
