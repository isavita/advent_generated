defmodule Day7 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def find_optimal_position(positions) do
    sorted_positions = Enum.sort(positions)
    median_index = div(length(sorted_positions), 2)
    Enum.at(sorted_positions, median_index)
  end

  def calculate_total_fuel_cost(positions, optimal_position) do
    Enum.reduce(positions, 0, fn position, acc ->
      acc + abs(position - optimal_position)
    end)
  end

  def call do
    positions = read_input()
    optimal_position = find_optimal_position(positions)
    total_fuel_cost = calculate_total_fuel_cost(positions, optimal_position)
    IO.puts("Total fuel cost: #{total_fuel_cost}")
  end
end

Day7.call()
