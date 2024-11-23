
defmodule Solution do
  def solve do
    valves = parse_input("input.txt")
    floyd_warshall(valves)
    |> max_pressure("AA", 30, 0, Enum.filter(valves, fn {_, v} -> v.flow > 0 end) |> Enum.map(fn {k, _} -> k end), 0)
  end

  def parse_input(path) do
    File.read!(path)
    |> String.trim()
    |> String.split("\n")
    |> Enum.reduce(%{}, fn line, acc ->
      [valve_info, tunnels_info] = String.split(line, "; ")
      [id, flow] = Regex.run(~r/Valve (\w+) has flow rate=(\d+)/, valve_info) |> tl()
      flow = String.to_integer(flow)
      
      tunnels = Regex.run(~r/tunnels? leads? to valves? (.+)/, tunnels_info)
      |> tl()
      |> hd()
      |> String.split(", ")
      
      Map.put(acc, id, %{id: id, flow: flow, tunnels: Map.new(tunnels, &{&1, 1})})
    end)
  end

  def floyd_warshall(valves) do
    Map.keys(valves)
    |> Enum.reduce(valves, fn k, acc ->
      Map.keys(acc)
      |> Enum.reduce(acc, fn i, acc1 ->
        Map.keys(acc1)
        |> Enum.reduce(acc1, fn j, acc2 ->
          dik = get_in(acc2, [i, :tunnels, k])
          dkj = get_in(acc2, [k, :tunnels, j])
          dij = get_in(acc2, [i, :tunnels, j])

          if dik && dkj && (!dij || dij > dik + dkj) do
            put_in(acc2, [i, :tunnels, j], dik + dkj)
          else
            acc2
          end
        end)
      end)
    end)
  end

  def max_pressure(valves, curr, minute, pressure, open, _depth) do
    Enum.reduce(open, pressure, fn next, max ->
      new_open = List.delete(open, next)
      time_left = minute - get_in(valves, [curr, :tunnels, next]) - 1

      if time_left > 0 do
        new_pressure = time_left * get_in(valves, [next, :flow]) + pressure
        max(max, max_pressure(valves, next, time_left, new_pressure, new_open, 0))
      else
        max
      end
    end)
  end

  def run do
    solve() |> IO.puts()
  end
end

Solution.run()
