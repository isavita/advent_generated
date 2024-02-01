
defmodule ReindeerOlympics do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_reindeer/1)
    |> Enum.map(&calculate_distance(&1, 2503))
    |> Enum.max()
  end

  defp parse_reindeer(line) do
    [speed, fly_time, rest_time] =
      Regex.scan(~r/\d+/, line)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)

    {speed, fly_time, rest_time}
  end

  defp calculate_distance({speed, fly_time, rest_time}, total_time) do
    cycle_time = fly_time + rest_time
    full_cycles = div(total_time, cycle_time)
    remaining_time = rem(total_time, cycle_time)
    distance_per_cycle = speed * fly_time
    total_distance = full_cycles * distance_per_cycle
    remaining_distance = speed * min(remaining_time, fly_time)

    total_distance + remaining_distance
  end
end
