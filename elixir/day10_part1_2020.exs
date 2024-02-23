defmodule Day10 do
  def read_input do
    "input.txt"
    |> File.stream!()
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_integer/1)
  end

  def calculate_differences(adapters) do
    {counts, _} = Enum.sort(adapters)
    |> Enum.reduce({%{1 => 0, 3 => 1}, 0}, fn adapter, {acc, last_adapter} ->
      diff = adapter - last_adapter
      {Map.update(acc, diff, 1, &(&1 + 1)), adapter}
    end)
    counts
  end

  def call do
    adapters = read_input()
    differences = calculate_differences([0 | adapters]) # Include the charging outlet
    result = Map.get(differences, 1) * Map.get(differences, 3)
    IO.puts("The number of 1-jolt differences multiplied by the number of 3-jolt differences: #{result}")
  end
end

Day10.call()
