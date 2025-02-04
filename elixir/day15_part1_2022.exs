
defmodule Solution do
  def main do
    sensors =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_sensor/1)

    IO.puts(impossible(sensors, 2000000))
  end

  defp parse_sensor(line) do
    [sx, sy, bx, by] =
      Regex.run(~r/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/, line, capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)

    {{sx, sy}, {bx, by}, manhattan({sx, sy}, {bx, by})}
  end

  defp impossible(sensors, y) do
    intervals =
      Enum.reduce(sensors, [], fn {{sx, sy}, _, dist}, acc ->
        dy = abs(sy - y)
        dx = dist - dy

        if dx >= 0 do
          [{sx - dx, sx + dx} | acc]
        else
          acc
        end
      end)
      |> Enum.sort()
      |> merge_intervals()
    
    beacons_at_y =
        sensors
        |> Enum.filter(fn {_, {_, by}, _} -> by == y end)
        |> Enum.map(fn {_, {bx, _}, _} -> bx end)
        |> MapSet.new()

    intervals
    |> Enum.map(fn {min, max} -> max - min + 1 end)
    |> Enum.sum()
    |> Kernel.-(Enum.count(beacons_at_y))
  end

  defp merge_intervals(intervals) do
    Enum.reduce(intervals, [], fn
      {min, max}, [] ->
        [{min, max}]

      {min, max}, [{pmin, pmax} | rest] ->
        if min <= pmax + 1 do
          [{pmin, max(pmax, max)} | rest]
        else
          [{min, max}, {pmin, pmax} | rest]
        end
    end)
    |> Enum.reverse()
  end

  defp manhattan({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end
end

Solution.main()
