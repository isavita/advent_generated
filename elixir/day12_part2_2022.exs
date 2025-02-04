
defmodule Solution do
  def solve do
    grid =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {char, x}, acc2 ->
          Map.put(acc2, {x, y}, char)
        end)
      end)

    {start, _} = Enum.find(grid, fn {_, v} -> v == ?S end)
    {finish, _} = Enum.find(grid, fn {_, v} -> v == ?E end)
    as = Enum.filter(grid, fn {_, v} -> v == ?a end) |> Enum.map(fn {k, _} -> k end)

    grid = Map.put(grid, start, ?a)
    grid = Map.put(grid, finish, ?z)

    dists = dijkstra(grid, finish)

    l = Map.get(dists, start)

    l =
      Enum.reduce(as, l, fn a, acc ->
        case Map.get(dists, a) do
          nil -> acc
          d -> min(acc, d)
        end
      end)

    IO.puts(l)
  end

  defp dijkstra(grid, finish) do
    queue = %{finish => 0}
    dist = %{finish => 0}
    dijkstra_helper(grid, queue, dist)
  end

  defp dijkstra_helper(_grid, queue, dist) when map_size(queue) == 0, do: dist

  defp dijkstra_helper(grid, queue, dist) do
    {{x, y}, current_dist} =
      queue
      |> Enum.min_by(fn {_, v} -> v end)

    queue = Map.delete(queue, {x, y})

    neighbors = [
      {x, y + 1},
      {x, y - 1},
      {x + 1, y},
      {x - 1, y}
    ]

    {queue, dist} =
      Enum.reduce(neighbors, {queue, dist}, fn next, {queue_acc, dist_acc} ->
        if Map.has_key?(grid, next) and grid[{x, y}] - grid[next] <= 1 do
          next_dist = current_dist + 1

          cond do
            !Map.has_key?(dist_acc, next) or next_dist < dist_acc[next] ->
              queue_acc = Map.put(queue_acc, next, next_dist)
              dist_acc = Map.put(dist_acc, next, next_dist)
              {queue_acc, dist_acc}

            true ->
              {queue_acc, dist_acc}
          end
        else
          {queue_acc, dist_acc}
        end
      end)

    dijkstra_helper(grid, queue, dist)
  end
end

Solution.solve()
