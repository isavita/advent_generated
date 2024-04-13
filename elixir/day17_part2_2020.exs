defmodule GameOfLife4D do
  defstruct x: 0, y: 0, z: 0, w: 0

  def main do
    input = File.read!("input.txt")
    active_cubes = parse_input(input)
    active_cubes = Enum.reduce(1..6, active_cubes, fn _, acc -> simulate_cycle(acc) end)
    IO.puts(Enum.count(active_cubes))
  end

  defp parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> char == "#" end)
      |> Enum.map(fn {_, x} -> %GameOfLife4D{x: x, y: y, z: 0, w: 0} end)
    end)
    |> MapSet.new()
  end

  defp simulate_cycle(active_cubes) do
    active_cubes
    |> Enum.reduce(%{}, fn cube, acc -> count_neighbors(cube, acc) end)
    |> Enum.reduce(MapSet.new(), fn {cube, count}, acc ->
      if count == 3 || (count == 2 && MapSet.member?(active_cubes, cube)) do
        MapSet.put(acc, cube)
      else
        acc
      end
    end)
  end

  defp count_neighbors(%GameOfLife4D{x: x, y: y, z: z, w: w}, acc) do
    for dw <- -1..1, dz <- -1..1, dy <- -1..1, dx <- -1..1, reduce: acc do
      acc ->
        unless dw == 0 && dz == 0 && dy == 0 && dx == 0 do
          neighbor = %GameOfLife4D{x: x + dx, y: y + dy, z: z + dz, w: w + dw}
          Map.update(acc, neighbor, 1, &(&1 + 1))
        else
          acc
        end
    end
  end
end

GameOfLife4D.main()