
defmodule SonarSweep do
  def call do
    input = File.read!("input.txt") |> String.split("\n") |> Enum.map(&String.to_integer/1)
    Enum.reduce(1..(length(input) - 1), 0, fn i, acc ->
      if Enum.at(input, i) > Enum.at(input, i - 1) do
        acc + 1
      else
        acc
      end
    end)
  end
end
