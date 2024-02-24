defmodule Day13 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> List.last()
    |> String.split(",")
    |> Enum.with_index()
    |> Enum.filter(fn {val, _} -> val != "x" end)
    |> Enum.map(fn {val, idx} -> {String.to_integer(val), idx} end)
  end

  defp earliest_timestamp(bus_schedule) do
    Enum.reduce(bus_schedule, {0, 1}, fn {bus_id, offset}, {start_time, step} ->
      {new_start_time, _} = Enum.reduce_while(0..10000000, {start_time, 0}, fn _, {acc_time, acc_step} ->
        if rem(acc_time + offset, bus_id) == 0 do
          if acc_step == 0 do
            {:cont, {acc_time, acc_time + step}}
          else
            {:halt, {acc_time, acc_step - acc_time}}
          end
        else
          {:cont, {acc_time + step, acc_step}}
        end
      end)
      {new_start_time, step * bus_id}
    end)
    |> elem(0)
  end

  def call do
    bus_schedule = read_input()
    timestamp = earliest_timestamp(bus_schedule)
    IO.puts("Earliest timestamp: #{timestamp}")
  end
end

Day13.call()
