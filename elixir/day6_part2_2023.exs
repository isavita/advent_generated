defmodule Day6Part2 do
  def call do
    file_path = "input.txt"
    {time, distance} = read_input(file_path)
    ways_to_win = calculate_ways_to_win(time, distance)
    IO.puts("Ways to beat the record: #{ways_to_win}")
  end

  defp read_input(file_path) do
    {:ok, content} = File.read(file_path)
    lines = String.split(content, "\n") |> Enum.reject(&(&1 == ""))

    time_line = Enum.find(lines, fn line -> String.starts_with?(line, "Time:") end)
    distance_line = Enum.find(lines, fn line -> String.starts_with?(line, "Distance:") end)

    time = parse_line(time_line)
    distance = parse_line(distance_line)

    {time, distance}
  end

  defp parse_line(line) do
    line
    |> String.replace(~r/[^0-9]/, "")
    |> String.to_integer()
  end

  defp calculate_ways_to_win(time, record) do
    # Since we're dealing with large numbers and a straightforward calculation might not be efficient,
    # we'll need to think about optimizing this calculation.
    # For a direct calculation approach:
    Enum.reduce(1..time-1, 0, fn hold_time, acc ->
      distance = hold_time * (time - hold_time)
      if distance > record, do: acc + 1, else: acc
    end)
    # Consider optimizing this logic for performance with large numbers.
  end
end

Day6Part2.call()
