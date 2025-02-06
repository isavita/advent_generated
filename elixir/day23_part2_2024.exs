
defmodule MaxClique do
  def solve(path) do
    graph =
      File.stream!(path)
      |> Stream.map(&String.trim/1)
      |> Stream.map(&String.split(&1, "-"))
      |> Stream.filter(&(length(&1) == 2))
      |> Enum.reduce(%{}, fn [a, b], acc ->
        acc
        |> Map.update(a, MapSet.new([b]), &MapSet.put(&1, b))
        |> Map.update(b, MapSet.new([a]), &MapSet.put(&1, a))
      end)

    nodes = Map.keys(graph) |> MapSet.new()

    {best_clique, _} =
      nodes
      |> MapSet.to_list()
      |> Enum.reduce({[], nodes}, fn node, {best_clique, candidates} ->
        find_clique(graph, [node], MapSet.intersection(candidates, graph[node] || MapSet.new()), MapSet.new(), best_clique)
      end)

    best_clique
    |> Enum.sort()
    |> Enum.join(",")
    |> IO.puts()
  end

    defp find_clique(graph, r, p, x, best_clique) do
    if MapSet.size(p) == 0 and MapSet.size(x) == 0 do
      if length(r) > length(best_clique) do
        {r, MapSet.new(Map.keys(graph))}
      else
        {best_clique, MapSet.new(Map.keys(graph))}
      end
    else
      p_list = MapSet.to_list(p)

      Enum.reduce(p_list, {best_clique, p}, fn v, {current_best_clique, current_p} ->
        neighbors = graph[v] || MapSet.new()

        {new_best_clique, _} =
          find_clique(
            graph,
            r ++ [v],
            MapSet.intersection(current_p, neighbors),
            MapSet.intersection(x, neighbors),
            current_best_clique
          )

        {new_best_clique, MapSet.delete(current_p, v)}
      end)
    end
  end
end

MaxClique.solve("input.txt")
