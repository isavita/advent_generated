
defmodule SmokeBasin do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.split("\n") |> Enum.map(&String.graphemes/1)

    sum_low_points_risk_levels(input)
  end

  defp sum_low_points_risk_levels(input) do
    input
    |> Enum.with_index()
    |> Enum.reduce(0, fn {row, i}, acc ->
      row
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {cell, j}, acc2 ->
        if is_low_point?(input, i, j), do: acc2 + 1 + String.to_integer(cell), else: acc2
      end)
    end)
  end

  defp is_low_point?(input, i, j) do
    cell = String.to_integer(Enum.at(Enum.at(input, i), j))

    neighbors = [
      {i - 1, j},
      {i + 1, j},
      {i, j - 1},
      {i, j + 1}
    ]

    Enum.all?(neighbors, fn {x, y} ->
      x < 0 or y < 0 or x >= length(input) or y >= length(Enum.at(input, 0)) or cell < String.to_integer(Enum.at(Enum.at(input, x), y))
    end)
  end
end
