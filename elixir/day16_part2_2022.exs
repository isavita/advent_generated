
defmodule Main do
  @moduledoc false

  def main do
    input = File.read!("input.txt") |> String.trim() |> String.split("\n")

    valves =
      input
      |> Enum.reduce(%{}, fn line, acc ->
        [id, flow, neighbors] = parse_line(line)
        Map.put(acc, id, %{flow: flow, neighbors: neighbors})
      end)

    distances = compute_distances(valves)

    open = valves |> Enum.filter(fn {_k, v} -> v.flow > 0 end) |> Enum.map(fn {k, _} -> k end)

    first = hd(open)
    subsets =
      subsets(open)
      |> Enum.filter(fn s -> Enum.member?(s, first) end)

    max =
      subsets
      |> Enum.reduce(0, fn subset, acc ->
        complement = open -- subset

        if complement == [] do
          acc
        else
          val1 = max_pressure(valves, distances, "AA", 26, subset)
          val2 = max_pressure(valves, distances, "AA", 26, complement)
          max(acc, val1 + val2)
        end
      end)

    IO.puts(max)
  end

  defp parse_line(line) do
    regex = ~r/Valve (\w+) has flow rate=(\d+); (?:tunnel leads to valve |tunnels lead to valves )(.+)/
    [_, id, flow, tunnels] = Regex.run(regex, line)
    flow = String.to_integer(flow)
    neighbors = String.split(tunnels, ", ")
    [id, flow, neighbors]
  end

  defp compute_distances(valves) do
    Enum.reduce(Map.keys(valves), %{}, fn start, acc ->
      Map.put(acc, start, bfs(valves, start))
    end)
  end

  defp bfs(valves, start) do
    queue = :queue.new()
    queue = :queue.in({start, 0}, queue)
    visited = MapSet.new([start])
    distances = %{start => 0}
    bfs_loop(queue, visited, distances, valves)
  end

  defp bfs_loop(queue, visited, distances, valves) do
    case :queue.out(queue) do
      {:empty, _} ->
        distances

      {{:value, {node, dist}}, queue} ->
        neighbors = valves[node].neighbors

        {queue, visited, distances} =
          Enum.reduce(neighbors, {queue, visited, distances}, fn nb, {q, v, d} ->
            if MapSet.member?(v, nb) do
              {q, v, d}
            else
              q = :queue.in({nb, dist + 1}, q)
              v = MapSet.put(v, nb)
              d = Map.put(d, nb, dist + 1)
              {q, v, d}
            end
          end)

        bfs_loop(queue, visited, distances, valves)
    end
  end

  defp max_pressure(valves, distances, curr, minute, open) do
    if minute <= 0 or open == [] do
      0
    else
      Enum.reduce(open, 0, fn next, acc ->
        travel = Map.get(distances[curr], next)
        time_left = minute - travel - 1

        if time_left > 0 do
          new_open = List.delete(open, next)
          val =
            max_pressure(valves, distances, next, time_left, new_open) +
              time_left * valves[next].flow

          max(acc, val)
        else
          acc
        end
      end)
    end
  end

  defp subsets([]), do: [[]]

  defp subsets([h | t]) do
    rest = subsets(t)
    rest ++ Enum.map(rest, fn s -> [h | s] end)
  end
end

Main.main()
