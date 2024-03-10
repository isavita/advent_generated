defmodule Day18 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp calculate_surface_area(droplet) do
    droplet
    |> Enum.reduce(0, fn [x, y, z], acc ->
      count = count_unconnected_sides(droplet, x, y, z)
      acc + count
    end)
  end

  defp count_unconnected_sides(droplet, x, y, z) do
    sides = [
      [x-1, y, z], [x+1, y, z],
      [x, y-1, z], [x, y+1, z],
      [x, y, z-1], [x, y, z+1]
    ]

    Enum.count(sides, fn [nx, ny, nz] ->
      not Enum.any?(droplet, &(&1 == [nx, ny, nz]))
    end)
  end

  def call do
    droplet = read_input()
    surface_area = calculate_surface_area(droplet)
    IO.puts("Surface Area: #{surface_area}")
  end
end

Day18.call()