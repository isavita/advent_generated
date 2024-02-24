defmodule Day7 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp fuel_cost(steps), do: div(steps * (steps + 1), 2)

  defp calculate_fuel_costs(positions, target_position) do
    Enum.map(positions, fn position ->
      steps = abs(position - target_position)
      fuel_cost(steps)
    end)
  end

  defp find_min_fuel(positions) do
    min_pos = Enum.min(positions)
    max_pos = Enum.max(positions)

    Enum.map(min_pos..max_pos, fn pos ->
      {pos, Enum.sum(calculate_fuel_costs(positions, pos))}
    end)
    |> Enum.min_by(fn {_pos, cost} -> cost end)
  end

  def call do
    positions = read_input()
    {_optimal_position, min_fuel_cost} = find_min_fuel(positions)
    IO.puts("Minimum fuel cost for alignment: #{min_fuel_cost}")
  end
end

Day7.call()
