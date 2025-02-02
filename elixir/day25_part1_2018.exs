
defmodule Day25 do
  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_point/1)
  end

  defp parse_point(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def manhattan_distance(p1, p2) do
    Enum.zip(p1, p2)
    |> Enum.map(fn {a, b} -> abs(a - b) end)
    |> Enum.sum()
  end

  def solve(points) do
    constellations = []

    Enum.reduce(points, constellations, fn point, acc ->
      merge_point(point, acc)
    end)
    |> length()
  end

  defp merge_point(point, constellations) do
    {merged, remaining} =
      Enum.split_with(constellations, fn constellation ->
        Enum.any?(constellation, & (manhattan_distance(point, &1) <= 3))
      end)

    new_constellation = [point | Enum.reduce(merged, [], &(&2 ++ &1))]
    [new_constellation | remaining]
  end
end

points = Day25.read_input("input.txt")
result = Day25.solve(points)
IO.puts(result)
