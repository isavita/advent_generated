defmodule TriangleValidator do
  def solve do
    # Read input from file
    input = read_input()

    # Parse the triangle sides from the input
    triangle_sides = parse_triangle_sides(input)

    # Validate the triangles
    valid_triangles = validate_triangles(triangle_sides)

    # Count the number of valid triangles
    count = length(valid_triangles)

    # Print the result
    IO.puts("Number of valid triangles: #{count}")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  defp parse_triangle_sides(input) do
    input
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.chunk_every(3)
    |> Enum.flat_map(fn chunk ->
      chunk
      |> Enum.zip()
      |> Enum.map(fn tuple -> Tuple.to_list(tuple) end)
    end)
  end

  defp validate_triangles(triangle_sides) do
    Enum.filter(triangle_sides, fn [a, b, c] ->
      a + b > c and a + c > b and b + c > a
    end)
  end
end

TriangleValidator.solve()
