
defmodule Day8 do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> char != ?. end)
      |> Enum.map(fn {char, x} -> {{x, y}, char} end)
    end)
    |> Enum.group_by(
      fn {_, char} -> char end,
      fn {{x, y}, _} -> {x, y} end
    )
  end

  def find_antinodes(coords) do
    coords
    |> Enum.filter(fn {_, locations} -> length(locations) >= 2 end)
    |> Enum.flat_map(fn {_, locations} ->
      for a <- locations, b <- locations, a != b do
        calculate_antinode(a, b)
      end
    end)
    |> List.flatten()
    |> Enum.uniq()
  end
    
  def calculate_antinode({x1, y1}, {x2, y2}) do
      # Antinode 1:  b is twice as far as a
      antinode1_x = 2 * x2 - x1
      antinode1_y = 2 * y2 - y1

      # Antinode 2: a is twice as far as b.
      antinode2_x = 2 * x1 - x2
      antinode2_y = 2 * y1 - y2
      
      [{antinode1_x, antinode1_y}, {antinode2_x, antinode2_y}]
  end


  def is_within_bounds(antinodes, width, height) do
      Enum.filter(antinodes, fn {x,y} ->
        x >= 0 and x < width and y >= 0 and y < height
      end)
  end

  def solve(filename) do
    antenna_map = read_input(filename)
    
    antinodes = find_antinodes(antenna_map)

    lines = String.split(File.read!(filename), "\n", trim: true)
    height = length(lines)
    width = if height > 0, do: String.length(Enum.at(lines, 0)), else: 0

    antinodes_in_bounds = is_within_bounds(antinodes, width, height)
    
    length(antinodes_in_bounds)
  end
end

result = Day8.solve("input.txt")
IO.puts(result)
