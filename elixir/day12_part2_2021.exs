
defmodule Solution do
  def solve(input) do
    parsed = parse_input(input)
    graph = build_graph(parsed)
    walk(graph, "start", %{"start" => 5}, ["start"], false)
  end

  defp build_graph(parsed) do
    Enum.reduce(parsed, %{}, fn [a, b], acc ->
      acc
      |> Map.put_new(a, MapSet.new())
      |> Map.put_new(b, MapSet.new())
      |> update_in([a], &MapSet.put(&1, b))
      |> update_in([b], &MapSet.put(&1, a))
    end)
  end

  defp walk(graph, current, visited, path, double_used) when current == "end" do
    1
  end

  defp walk(graph, current, visited, path, double_used) do
    visited = Map.update(visited, current, 1, &(&1 + 1))

    graph
    |> Map.get(current, MapSet.new())
    |> Enum.reduce(0, fn next, acc ->
      cond do
        next == "start" ->
          acc

        String.upcase(next) != next and Map.get(visited, next, 0) > 0 ->
          if double_used do
            acc
          else
            walk(graph, next, visited, [next | path], true) + acc
          end

        true ->
          walk(graph, next, visited, [next | path], double_used) + acc
      end
    end)
  end

  defp parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, "-"))
  end

  def main do
    {:ok, input} = File.read("input.txt")
    result = solve(input)
    IO.puts(result)
  end
end

Solution.main()
