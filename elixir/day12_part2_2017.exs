
defmodule DigitalPlumber do
  def run do
    input = File.read!("input.txt")
    connections = parse_input(input)

    group_count = count_groups(connections)
    group_size = count_group_size(connections, 0)

    IO.puts("Number of programs in the group containing program ID 0: #{group_size}")
    IO.puts("Total number of groups: #{group_count}")
  end

  defp parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, acc ->
      [program, connections] = String.split(line, " <-> ")
      connections = String.split(connections, ", ") |> Enum.map(&String.to_integer/1)
      Map.update(acc, String.to_integer(program), connections, &(&1 ++ connections))
    end)
  end

  defp count_group_size(connections, start) do
    connections
    |> dfs(start, MapSet.new())
    |> MapSet.size()
  end

  defp dfs(connections, current, visited) do
    if MapSet.member?(visited, current) do
      visited
    else
      visited = MapSet.put(visited, current)
      neighbors = Map.get(connections, current, [])
      Enum.reduce(neighbors, visited, fn neighbor, acc -> dfs(connections, neighbor, acc) end)
    end
  end

  defp count_groups(connections) do
    all_programs = Map.keys(connections)
    Enum.reduce(all_programs, {0, MapSet.new()}, fn program, {count, visited} ->
      if MapSet.member?(visited, program) do
        {count, visited}
      else
        new_visited = dfs(connections, program, visited)
        {count + 1, new_visited}
      end
    end)
    |> elem(0)
  end
end

DigitalPlumber.run()
