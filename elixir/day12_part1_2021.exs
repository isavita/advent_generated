defmodule CaveSystem do
  defstruct connections: %{}

  def new(), do: %CaveSystem{}

  def connect_to(%CaveSystem{connections: connections} = cave, name) do
    %{cave | connections: Map.put(connections, name, true)}
  end
end

defmodule Main do
  def run do
    "input.txt"
    |> File.stream!()
    |> Enum.reduce(%{}, fn line, caves ->
      [from, to] = String.trim(line) |> String.split("-")
      caves
      |> ensure_cave(from)
      |> ensure_cave(to)
      |> Map.update!(from, &CaveSystem.connect_to(&1, to))
      |> Map.update!(to, &CaveSystem.connect_to(&1, from))
    end)
    |> count_paths("start", %{"start" => true})
    |> IO.puts()
  end

  defp ensure_cave(caves, name) do
    Map.put_new(caves, name, CaveSystem.new())
  end

  defp count_paths(caves, "end", _visited), do: 1

  defp count_paths(caves, current, visited) do
    caves[current].connections
    |> Enum.filter(fn {next, _} -> !Map.get(visited, next, false) or String.upcase(next) == next end)
    |> Enum.reduce(0, fn {next, _}, acc ->
      acc + count_paths(caves, next, Map.put(visited, next, true))
    end)
  end
end

Main.run()