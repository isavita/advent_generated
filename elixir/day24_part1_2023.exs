
defmodule Day24 do
  def read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_hailstone/1)
  end

  defp parse_hailstone(line) do
    [px, py, pz, vx, vy, vz] =
      line
      |> String.replace("@", ",")
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)

    {px, py, pz, vx, vy, vz}
  end

  defp intersection(h1, h2) do
    {px1, py1, _pz1, vx1, vy1, _vz1} = h1
    {px2, py2, _pz2, vx2, vy2, _vz2} = h2

    denominator = vx1 * vy2 - vy1 * vx2

    if denominator == 0 do
      nil # Parallel lines
    else
      t2 = ((py1 - py2) * vx1 - (px1 - px2) * vy1) / denominator
      t1 = (px2 - px1 + vx2 * t2) / vx1
      
      if t1 > 0 and t2 > 0 do
        x = px1 + vx1 * t1
        y = py1 + vy1 * t1
        {x, y}
      else
        nil
      end
    end
  end
  
  def count_intersections(hailstones, min, max) do
    hailstones
    |> Enum.chunk_every(2, 1, :discard)  # Generate all pairs without repeating combinations
    |> Enum.filter(fn [h1, h2] -> h1 != h2 end) #filter hailstones pairs that contains the same hailstone.
    |> Enum.reduce(0, fn [h1, h2], acc ->
      case intersection(h1, h2) do
        nil -> acc
        {x, y} ->
          if x >= min and x <= max and y >= min and y <= max do
            acc + 1
          else
            acc
          end
      end
    end)
  end

  def solve(filename, min, max) do
    hailstones = read_input(filename)

    # Get all unique pairs (combinations)
    pairs = for i <- 0..(length(hailstones) - 2), j <- (i + 1)..(length(hailstones) - 1),
        do: [Enum.at(hailstones, i), Enum.at(hailstones, j)]

    pairs
    |> Enum.reduce(0, fn [h1, h2], acc ->
        case intersection(h1, h2) do
          nil -> acc
          {x, y} ->
            if x >= min and x <= max and y >= min and y <= max do
              acc + 1
            else
              acc
            end
        end
    end)
  end
  
end


# Example usage (for the provided example and part 1):
# result = Day24.count_intersections(Day24.read_input("input.txt"), 7, 27) # example test
result = Day24.solve("input.txt", 200000000000000, 400000000000000) # part 1 data
IO.puts(result)

