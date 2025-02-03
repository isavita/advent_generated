
defmodule Solution do
  def read_input(file) do
    File.read!(file)
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, acc ->
      case String.split(line, "-") do
        [c1, c2] ->
          acc
          |> Map.update(c1, MapSet.new([c2]), &MapSet.put(&1, c2))
          |> Map.update(c2, MapSet.new([c1]), &MapSet.put(&1, c1))
        _ ->
          acc
      end
    end)
  end

  def find_triplets(graph) do
    computers = Map.keys(graph)

    for c1 <- computers,
        c2 <- computers,
        c2 > c1,
        c3 <- computers,
        c3 > c2,
        MapSet.member?(graph[c1], c2) and MapSet.member?(graph[c2], c3) and
          MapSet.member?(graph[c1], c3) and
          (String.starts_with?(c1, "t") or String.starts_with?(c2, "t") or
             String.starts_with?(c3, "t")),
        do: [c1, c2, c3],
        into: MapSet.new()
  end

  def main do
    graph = read_input("input.txt")
    triplets = find_triplets(graph)
    IO.puts("Number of triplets containing at least one computer with name starting with 't': #{MapSet.size(triplets)}")
  end
end

Solution.main()
