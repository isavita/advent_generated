defmodule Day13 do
  def read_input do
    [timestamp, buses] = File.read!("input.txt") |> String.trim() |> String.split("\n")
    {String.to_integer(timestamp), Enum.filter(String.split(buses, ","), &(&1 != "x")) |> Enum.map(&String.to_integer/1)}
  end

  defp earliest_bus(timestamp, bus_ids) do
    bus_ids
    |> Enum.map(fn id -> {id, id - rem(timestamp, id)} end) # Calculate wait time for each bus
    |> Enum.min_by(fn {_id, wait_time} -> wait_time end) # Find the bus with the minimum wait time
  end

  def call do
    {timestamp, bus_ids} = read_input()
    {earliest_bus_id, wait_time} = earliest_bus(timestamp, bus_ids)
    result = earliest_bus_id * wait_time

    IO.puts("ID of the earliest bus multiplied by the wait time: #{result}")
  end
end

Day13.call()
