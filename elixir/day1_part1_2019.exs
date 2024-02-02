
defmodule RocketEquation do
  def call do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&calculate_fuel(&1))
    |> Enum.sum()
  end

  defp calculate_fuel(mass) do
    mass
    |> String.to_integer()
    |> div(3)
    |> Kernel.- 2
  end
end
