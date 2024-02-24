defmodule Day6 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp initialize_counts(fish_timers) do
    # Initialize a map with keys 0 through 8, all set to 0
    initial_counts = Map.new(Enum.map(0..8, fn x -> {x, 0} end))

    # Increment the count for each timer value in the list
    Enum.reduce(fish_timers, initial_counts, fn timer, acc ->
      Map.update!(acc, timer, &(&1 + 1))
    end)
  end

  def simulate_day(fish_counts) do
    new_fish = Map.get(fish_counts, 0, 0)

    updated_counts = 
      Enum.reduce(1..8, %{}, fn timer, acc ->
        count = Map.get(fish_counts, timer, 0)
        Map.put(acc, timer - 1, count)
      end)

    updated_counts
    |> Map.update!(6, &(&1 + new_fish)) # Reset timers to 6 for fish that spawned new ones
    |> Map.put(8, new_fish) # Add new fish with timer 8
  end

  def simulate_days(fish_counts, 0), do: fish_counts
  def simulate_days(fish_counts, days) do
    simulate_days(simulate_day(fish_counts), days - 1)
  end

  def count_fish(fish_counts) do
    fish_counts |> Map.values() |> Enum.sum()
  end

  def call do
    fish_timers = read_input()
    fish_counts = initialize_counts(fish_timers)
    final_counts = simulate_days(fish_counts, 80)
    IO.puts("Total lanternfish after 80 days: #{count_fish(final_counts)}")
  end
end

Day6.call()
