defmodule Nanobots do
  def main do
    "input.txt"
    |> File.read!()
    |> parse_nanobots()
    |> process_nanobots()
    |> IO.puts()
  end

  def parse_nanobots(contents) do
    regex = ~r/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/
    Regex.scan(regex, contents)
    |> Enum.map(fn [_, x, y, z, r] -> 
      %{x: String.to_integer(x), y: String.to_integer(y), z: String.to_integer(z), radius: String.to_integer(r)}
    end)
  end

  def process_nanobots(nanobots) do
    strongest = find_strongest_nanobot(nanobots)
    count_nanobots_in_range(nanobots, strongest)
  end

  def find_strongest_nanobot(nanobots) do
    Enum.max_by(nanobots, &(&1.radius))
  end

  def count_nanobots_in_range(nanobots, strongest) do
    Enum.count(nanobots, fn nanobot ->
      manhattan_distance(nanobot, strongest) <= strongest.radius
    end)
  end

  def manhattan_distance(a, b) do
    local_abs(a.x - b.x) + local_abs(a.y - b.y) + local_abs(a.z - b.z)
  end

  defp local_abs(x) when x < 0, do: -x
  defp local_abs(x), do: x
end

Nanobots.main()