
defmodule TriangleValidator do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.filter(&valid_triangle?/1)
    |> Enum.count()
  end

  defp valid_triangle?(triangle) do
    sides = triangle |> String.split() |> Enum.map(&String.to_integer/1) |> Enum.sort()
    Enum.at(sides, 0) + Enum.at(sides, 1) > Enum.at(sides, 2)
  end
end
