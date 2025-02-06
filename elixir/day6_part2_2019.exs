
defmodule Orbit do
  def build_orbit_map(path) do
    File.stream!(path)
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.split(&1, ")"))
    |> Enum.reduce(%{}, fn [center, orbiter], acc ->
      center_node = Map.get(acc, center, %{name: center, children: [], parent: nil})
      orbiter_node = Map.get(acc, orbiter, %{name: orbiter, children: [], parent: nil})

      orbiter_node = %{orbiter_node | parent: center}
      center_node = %{center_node | children: [orbiter | center_node.children]}

      acc
      |> Map.put(center, center_node)
      |> Map.put(orbiter, %{orbiter_node | parent: center_node.name})
    end)
  end

  def path_to_root(orbit_map, node_name) do
    Stream.unfold(node_name, fn
      nil -> nil
      name -> {name, orbit_map[name].parent}
    end)
    |> Enum.to_list()
    |> Enum.reverse()
  end

  def find_common_ancestor(orbit_map, node1, node2) do
    path1 = path_to_root(orbit_map, node1)
    path2 = path_to_root(orbit_map, node2)

    {common_length, _, _} =
      Enum.zip(path1, path2)
      |> Enum.reduce_while({0, path1, path2}, fn {n1, n2}, {count, _, _} ->
        if n1 == n2 do
          {:cont, {count + 1, path1, path2}}
        else
          {:halt, {count, path1, path2}}
        end
      end)
      
      length1 = length(path1)
      length2 = length(path2)

    (length1-common_length) + (length2 - common_length)
  end

  def solve(path) do
    orbit_map = build_orbit_map(path)
    find_common_ancestor(orbit_map, orbit_map["YOU"].parent, orbit_map["SAN"].parent)
  end
end

Orbit.solve("input.txt") |> IO.puts()
