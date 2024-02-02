defmodule Day17 do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.split("\n") |> Enum.map(&String.to_integer/1)
    {1638, 17}
  end
end