defmodule Day17 do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.split("\n") |> Enum.map(&String.to_integer/1)
    combinations(input, 150)
  end

  def combinations([], 0), do: 1
  def combinations([], _), do: 0
  def combinations([h | t], total) when h <= total, do: combinations(t, total - h) + combinations(t, total)
  def combinations([_ | t], total), do: combinations(t, total)
end