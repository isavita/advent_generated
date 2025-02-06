
defmodule Day15 do
  def parse_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      Regex.scan(~r/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/, line, capture: :all_but_first)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)
      |> then(fn [sx, sy, bx, by] ->
        {{sx, sy}, {bx, by}}
      end)
    end)
  end

  def manhattan_distance({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  def solve_part1(sensors_and_beacons, target_y) do
    covered = MapSet.new()
    beacons_on_target = MapSet.new()

    sensors_and_beacons
    |> Enum.each(fn {{sx, sy}, {bx, by}} ->
      distance = manhattan_distance({sx, sy}, {bx, by})
      dy = abs(sy - target_y)

      if by == target_y do
        beacons_on_target = MapSet.put(beacons_on_target, bx)
      end

      if dy <= distance do
        dx = distance - dy
        (sx - dx..sx + dx)
        |> Enum.each(fn x ->
          covered = MapSet.put(covered, x)
        end)
      end
    end)
    MapSet.difference(covered, beacons_on_target) |> MapSet.size()
  end

  def solve_part2(sensors_and_beacons, max_coord) do
    find_beacon(sensors_and_beacons, max_coord)
  end

  def find_beacon(sensors_and_beacons, max_coord) do
    0..max_coord
    |> Enum.find_value(fn y ->
      ranges =
        sensors_and_beacons
        |> Enum.map(fn {{sx, sy}, {bx, by}} ->
          distance = manhattan_distance({sx, sy}, {bx, by})
          dy = abs(sy - y)

          if dy <= distance do
            dx = distance - dy
            {max(0, sx - dx), min(max_coord, sx + dx)}
          else
            nil
          end
        end)
        |> Enum.filter(& &1)
        |> Enum.sort_by(fn {start, _} -> start end)

      find_gap(ranges, 0, max_coord, y)
    end)
  end

  def find_gap([], _current_max, _max_coord, _y), do: nil

  def find_gap([{start, finish} | rest], current_max, max_coord, y) do
    if start > current_max + 1 do
      {current_max + 1, y}
      |> then(fn {x, y} -> x * 4000000 + y end)
    else
      new_current_max = max(current_max, finish)

      if new_current_max == max_coord do
        nil
      else
        find_gap(rest, new_current_max, max_coord, y)
      end
    end
  end
end

input = Day15.parse_input("input.txt")
part1_result = Day15.solve_part1(input, 2000000)
IO.puts("Part 1: #{part1_result}")

part2_result = Day15.solve_part2(input, 4000000)
IO.puts("Part 2: #{part2_result}")
