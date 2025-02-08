
defmodule Day8 do
  def read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
  end

  def find_antennas(grid) do
    Enum.with_index(grid)
    |> Enum.reduce(%{}, fn {row, y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {char, x}, inner_acc ->
        if char != ?. do
          Map.update(inner_acc, char, [{x, y}], &(&1 ++ [{x, y}]))
        else
          inner_acc
        end
      end)
    end)
  end

  def calculate_antinodes(grid, antennas, part) do
    height = length(grid)
    width = length(Enum.at(grid, 0))
    
    all_antinodes =
      Enum.reduce(antennas, MapSet.new(), fn {_, positions}, acc ->
          antinodes_for_frequency = calculate_antinodes_for_frequency(positions, width, height, part)
          MapSet.union(acc, antinodes_for_frequency)
      end)

    MapSet.size(all_antinodes)
  end

  defp calculate_antinodes_for_frequency(positions, width, height, part) do
    cond do
      length(positions) < 2 ->
        MapSet.new()

      part == 1 ->
        calculate_antinodes_part1(positions, width, height)

      part == 2 ->
        calculate_antinodes_part2(positions, width, height)
    end
  end

  defp calculate_antinodes_part1(positions, width, height) do
      combinations = for p1 <- positions, p2 <- positions, p1 != p2, do: {p1, p2}
      Enum.reduce(combinations, MapSet.new, fn({p1,p2},acc) ->
          {x1, y1} = p1
          {x2, y2} = p2
          dx = x2 - x1
          dy = y2 - y1

          antinode1 = {x1 - dx, y1 - dy}
          antinode2 = {x2 + dx, y2 + dy}
          
          acc
          |> add_if_in_bounds(antinode1, width, height)
          |> add_if_in_bounds(antinode2, width, height)          
      end)
  end

  defp calculate_antinodes_part2(positions, width, height) do
    antinodes = MapSet.new(positions) # Antennas are also antinodes in part 2

    combinations = for p1 <- positions, p2 <- positions, p1 != p2, do: {p1, p2}
        
    Enum.reduce(combinations, antinodes, fn {p1, p2}, acc ->
      {x1, y1} = p1
      {x2, y2} = p2

      dx = x2 - x1
      dy = y2 - y1

      # Generate all points along the line defined by p1 and p2
      
      all_points = generate_points_on_line(p1,p2,width,height)
      Enum.reduce(all_points, acc, fn(point,inner_acc) ->
          add_if_in_bounds(inner_acc, point,width,height)
      end)
    end)
  end

  defp generate_points_on_line({x1,y1},{x2,y2},width,height) do
    dx = x2 - x1
    dy = y2 - y1

    gcd = gcd(abs(dx), abs(dy))
    dx = div(dx, gcd)
    dy = div(dy, gcd)
    
    points = for i <- -100..100 do # check large range
      {x1 + i * dx, y1 + i * dy}       
    end
    points
  end


  defp add_if_in_bounds(set, {x, y}, width, height) do
    if x >= 0 and x < width and y >= 0 and y < height do
      MapSet.put(set, {x, y})
    else
      set
    end
  end
  
  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))

  def part1(filename) do
    grid = read_input(filename)
    antennas = find_antennas(grid)
    calculate_antinodes(grid, antennas, 1)
  end

  def part2(filename) do
    grid = read_input(filename)
    antennas = find_antennas(grid)
    calculate_antinodes(grid, antennas, 2)
  end
end

# Run the program
filename = "input.txt"
IO.puts("Part 1: #{Day8.part1(filename)}")
IO.puts("Part 2: #{Day8.part2(filename)}")
