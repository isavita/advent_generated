
defmodule DigitalPlumber do
  def call do
    input = File.read!("input.txt")
    |> String.split("\n")
    |> Enum.map(&String.split(&1, " <-> "))
    |> Enum.map(fn [prog, conns] -> {String.to_integer(prog), String.split(conns, ", ") |> Enum.map(&String.to_integer/1)} end)

    graph = Map.new(input)

    group = bfs(graph, 0, [])

    length(group)
  end

  def bfs(graph, start, visited) do
    case Map.get(graph, start) do
      nil -> visited
      conns ->
        new_visited = visited ++ [start]
        new_nodes = conns -- new_visited
        Enum.reduce(new_nodes, new_visited, fn node, acc -> bfs(graph, node, acc) end)
    end
  end
end
