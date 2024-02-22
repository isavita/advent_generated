defmodule Day4 do
  def call do
    input = read_input("input.txt")
    sorted_logs = Enum.sort(input)
    {sleep_schedule, _} = process_logs(sorted_logs, %{}, nil, nil, 0)

    {guard_id, most_minute} = find_most_frequently_asleep_guard_and_minute(sleep_schedule)

    IO.puts("Guard ##{guard_id} is most frequently asleep on the same minute #{most_minute}.")
    IO.puts("Answer: #{guard_id * most_minute}")
  end

  defp read_input(file_path) do
    File.stream!(file_path)
    |> Enum.map(&String.trim/1)
  end

  defp process_logs([], sleep_schedule, _, _, _), do: {sleep_schedule, nil}
  defp process_logs([log | rest], sleep_schedule, current_guard, asleep_since, _minute) do
    case Regex.run(~r/\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] (.+)/, log) do
      [_, minute_string, "falls asleep"] ->
        process_logs(rest, sleep_schedule, current_guard, String.to_integer(minute_string), 0)
      [_, minute_string, "wakes up"] ->
        new_minute = String.to_integer(minute_string)
        updated_schedule = update_sleep_schedule(sleep_schedule, current_guard, asleep_since, new_minute)
        process_logs(rest, updated_schedule, current_guard, nil, 0)
      [_, minute_string, info] ->
        new_minute = String.to_integer(minute_string)
        {new_guard, _} = parse_guard_info(info)
        process_logs(rest, sleep_schedule, new_guard, nil, new_minute)
    end
  end

  defp parse_guard_info(info) do
    case Regex.run(~r/Guard #(\d+) begins shift/, info) do
      nil -> {nil, info}
      [_, id] -> {String.to_integer(id), nil}
    end
  end

  defp update_sleep_schedule(sleep_schedule, guard_id, start_minute, end_minute) do
    minutes_asleep = Enum.map(start_minute..end_minute-1, &(&1))
    Map.update(sleep_schedule, guard_id, minutes_asleep, fn existing ->
      existing ++ minutes_asleep
    end)
  end

  defp find_most_frequently_asleep_guard_and_minute(sleep_schedule) do
    sleep_schedule
    |> Enum.map(fn {guard_id, minutes} ->
      {guard_id, most_common_minute(minutes)}
    end)
    |> Enum.max_by(fn {_guard_id, {_minute, count}} -> count end)
    |> case do
      {guard_id, {most_minute, _}} -> {guard_id, most_minute}
      _ -> {nil, 0}
    end
  end

  defp most_common_minute(minutes) do
    minutes
    |> Enum.reduce(%{}, fn minute, acc ->
      Map.update(acc, minute, 1, &(&1 + 1))
    end)
    |> Enum.max_by(fn {_minute, count} -> count end, fn -> {nil, 0} end)
    |> case do
      {minute, count} when is_integer(minute) -> {minute, count}
      _ -> {nil, 0} # Handle the case where there is no common minute
    end
  end
end

Day4.call()
