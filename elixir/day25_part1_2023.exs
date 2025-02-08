
defmodule Solver do
  def parse_input(input) do
    Enum.reduce(input, %{}, fn line, acc ->
      [vertice | others] = String.split(line, [": ", " "])

      Enum.reduce(others, Map.put_new(acc, vertice, MapSet.new()), fn other, graph ->
        graph
        |> Map.put_new(other, MapSet.new())
        |> Map.update!(vertice, &MapSet.put(&1, other))
        |> Map.update!(other, &MapSet.put(&1, vertice))
      end)
    end)
  end

  def bfs(graph, start, goal_func, reached, came_from) do
    do_bfs(graph, [start], reached, came_from, goal_func)
  end

  defp do_bfs(graph, [], reached, came_from, _goal_func), do: {false, reached, came_from}

  defp do_bfs(graph, [current | frontier], reached, came_from, goal_func) do
    if goal_func.(current) do
      {true, reached, came_from}
    else
      new_frontier =
        graph
        |> Map.get(current, MapSet.new())
        |> MapSet.to_list()
        |> Enum.flat_map(fn next ->
          if MapSet.member?(reached, next) do
            []
          else
            [next]
          end
        end)

      new_reached =
        Enum.reduce(new_frontier, reached, fn x, acc ->
          MapSet.put(acc, x)
        end)

      new_came_from =
        Enum.reduce(new_frontier, came_from, fn next, acc ->
          Map.put(acc, next, current)
        end)

      do_bfs(graph, frontier ++ new_frontier, new_reached, new_came_from, goal_func)
    end
  end

  def reconstruct_path(start, end_node, came_from) do
    do_reconstruct_path(end_node, start, came_from, [])
    |> Enum.reverse()
  end

  defp do_reconstruct_path(current, start, came_from, path) when current == start,
    do: [start | path]

  defp do_reconstruct_path(current, start, came_from, path) do
    do_reconstruct_path(came_from[current], start, came_from, [current | path])
  end

  def solve(input) do
    min_cut = 3
    graph = parse_input(input)
    source = graph |> Map.keys() |> List.first()

    separate_graph =
      Enum.reduce_while(Map.keys(graph), graph, fn end_node, acc_graph ->
        if source == end_node do
          {:cont, acc_graph}
        else
          new_graph =
            Enum.reduce(1..min_cut, acc_graph, fn _, g ->
              {_, _, came_from} =
                bfs(g, source, fn vertice -> vertice == end_node end, MapSet.new([source]), %{
                  source => source
                })

              path = reconstruct_path(source, end_node, came_from)

              Enum.reduce(Enum.chunk_every(path, 2, 1, :discard), g, fn [u, v], acc ->
                acc
                |> Map.update!(u, &MapSet.delete(&1, v))
                |> Map.update!(v, &MapSet.delete(&1, u))
              end)
            end)

          {is_valid, _, _} =
            bfs(
              new_graph,
              source,
              fn vertice -> vertice == end_node end,
              MapSet.new([source]),
              %{source => source}
            )

          if !is_valid do
            {:halt, new_graph}
          else
            {:cont, acc_graph}
          end
        end
      end)

    {_, reached, _} =
      bfs(separate_graph, source, fn _ -> false end, MapSet.new([source]), %{source => source})

    length1 = MapSet.size(reached)
    length2 = map_size(separate_graph) - length1
    length1 * length2
  end

  def read_file(file_name) do
    file_name
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
  end
end

Solver.read_file("input.txt") |> Solver.solve() |> IO.puts()
