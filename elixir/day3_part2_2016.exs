defmodule PossibleTrianglesVerticalLayout do
  def call() do
    "input.txt"
    |> File.read!()
    |> possible_triangles_vertical_layout()
  end
  defp possible_triangles_vertical_layout(input), do: count_possible_triangles(input, &match_triples_vertically/1)

  defp count_possible_triangles(input, triplet_matcher) do
    input
    |> String.split
    |> Enum.map(&String.to_integer/1)
    |> triplet_matcher.()
    |> Enum.count(&possible_triangle?/1)
  end

  defp match_triples_vertically(int_list) do
    int_list
    |> Enum.chunk(3)
    |> Enum.chunk(3)
    |> Enum.flat_map(fn [[a,b,c], [d,e,f], [g,h,i]] ->
      [[a,d,g], [b,e,h], [c,f,i]]
    end)
  end

  defp possible_triangle?([a, b, c]) when a+b>c and a+c>b and b+c>a, do: true
  defp possible_triangle?(_), do: false
end
