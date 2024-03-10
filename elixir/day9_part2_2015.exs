defmodule LongestRoute do
  def solve do
    # Read input from file
    input = read_input()

    # Parse distances from input
    distances = parse_distances(input)

    # Get all possible routes
    routes = get_all_routes(distances)

    # Find the longest route
    longest_distance = find_longest_distance(routes, distances)

    # Print the result
    IO.puts("The longest distance is #{longest_distance}.")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  defp parse_distances(input) do
    Enum.reduce(input, %{}, fn line, acc ->
      [city1, "to", city2, "=", distance] = String.split(line)
      distance = String.to_integer(distance)
      Map.put(acc, {city1, city2}, distance)
      |> Map.put({city2, city1}, distance)
    end)
  end

  defp get_all_routes(distances) do
    distances
    |> Map.keys()
    |> Enum.map(fn {city1, _} -> city1 end)
    |> Enum.uniq()
    |> permutations()
  end

  defp permutations([]), do: [[]]
  defp permutations(list) do
    for head <- list, tail <- permutations(list -- [head]), do: [head | tail]
  end

  defp find_longest_distance(routes, distances) do
    Enum.reduce(routes, 0, fn route, longest_distance ->
      distance = calculate_route_distance(route, distances)
      max(distance, longest_distance)
    end)
  end

  defp calculate_route_distance(route, distances) do
    route
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [city1, city2] -> distances[{city1, city2}] end)
    |> Enum.sum()
  end
end

LongestRoute.solve()
