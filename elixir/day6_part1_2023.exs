defmodule Day6 do
  def call do
    file_path = "input.txt" # Ensure this variable is correctly set
    {times, distances} = read_input(file_path)
    ways_to_win = Enum.zip(times, distances)
                  |> Enum.map(fn {time, distance} -> calculate_ways_to_win(time, distance) end)
    answer = Enum.reduce(ways_to_win, 1, &(&1 * &2))
    IO.puts("Total ways to beat the records multiplied together: #{answer}")
  end

  defp read_input(file_path) do
    {:ok, content} = File.read(file_path)
    lines = String.split(content, "\n")
    times_line = Enum.at(lines, 0)
    distances_line = Enum.at(lines, 1)

    times = times_line
            |> String.replace("Time:", "")
            |> String.trim()
            |> String.split(~r/\s+/, trim: true)
            |> Enum.map(&String.to_integer/1)

    distances = distances_line
                |> String.replace("Distance:", "")
                |> String.trim()
                |> String.split(~r/\s+/, trim: true)
                |> Enum.map(&String.to_integer/1)

    {times, distances}
  end

  defp calculate_ways_to_win(time, record) do
    Enum.count(0..time-1, fn hold_time ->
      distance = hold_time * (time - hold_time)
      distance > record
    end)
  end
end

Day6.call()
