
defmodule RocketEquation do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
    total_fuel = Enum.reduce(input, 0, fn mass, acc ->
      calculate_fuel(mass, 0) + acc
    end)
    total_fuel
  end

  defp calculate_fuel(mass, acc) when mass <= 0, do: acc
  defp calculate_fuel(mass, acc) do
    fuel = div(mass, 3) - 2
    calculate_fuel(fuel, max(fuel, 0) + acc)
  end
end
