defmodule Day10 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
    |> Enum.sort()
  end

  def count_arrangements(adapters) do
    # Add the starting point (0) and the device's built-in adapter (max + 3)
    extended_adapters = [0 | adapters] ++ [Enum.max(adapters) + 3]

    # Initialize the map with the starting point having a count of 1
    counts =
      Enum.reduce(extended_adapters, %{0 => 1}, fn adapter, acc ->
        # Calculate the sum of paths leading to this adapter
        sum =
          Enum.sum(
            for prev <-
                  Enum.slice(
                    extended_adapters,
                    0..(Enum.find_index(extended_adapters, &(&1 == adapter)) - 1)
                  ),
                do: Map.get(acc, prev, 0) * if(adapter - prev <= 3, do: 1, else: 0)
          )

        Map.put(acc, adapter, sum)
      end)

    # The total number of arrangements is the count at the last adapter
    Map.get(counts, Enum.max(extended_adapters))
  end

  def call do
    adapters = read_input()
    arrangements = count_arrangements(adapters)
    IO.puts("Total number of distinct ways: #{arrangements}")
  end
end

Day10.call()
