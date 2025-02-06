
defmodule Solution do
  def run() do
    {:ok, input} = File.read("input.txt")

    cubes =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_tuple/1)
      |> MapSet.new()

    {min, max} = find_min_max(cubes)
    min = add_tuples(min, {-1, -1, -1})
    max = add_tuples(max, {1, 1, 1})

    IO.puts(bfs(cubes, min, max))
  end

  defp parse_tuple(line) do
    [x, y, z] =
      line
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    {x, y, z}
  end

  defp find_min_max(cubes) do
    Enum.reduce(cubes, {{1000, 1000, 1000}, {-1000, -1000, -1000}}, fn {x, y, z}, {min, max} ->
      {{min(x, elem(min, 0)), min(y, elem(min, 1)), min(z, elem(min, 2))},
       {max(x, elem(max, 0)), max(y, elem(max, 1)), max(z, elem(max, 2))}}
    end)
  end

  defp add_tuples({x1, y1, z1}, {x2, y2, z2}) do
    {x1 + x2, y1 + y2, z1 + z2}
  end

  defp bfs(cubes, min, max) do
    neighbors = [
      {-1, 0, 0},
      {1, 0, 0},
      {0, -1, 0},
      {0, 1, 0},
      {0, 0, -1},
      {0, 0, 1}
    ]

    q = :queue.from_list([min])
    seen = MapSet.new([min])

    bfs_helper(q, seen, cubes, neighbors, min, max, 0)
  end

  defp bfs_helper(q, seen, cubes, neighbors, min, max, faces) do
    case :queue.out(q) do
      {{:value, curr}, q} ->
        neighbors
        |> Enum.reduce({q, seen, faces}, fn delta, {q_acc, seen_acc, faces_acc} ->
          next_t = add_tuples(curr, delta)

          cond do
            out_of_bounds(next_t, min, max) ->
              {q_acc, seen_acc, faces_acc}

            next_t in cubes ->
              {q_acc, seen_acc, faces_acc + 1}

            next_t in seen_acc ->
              {q_acc, seen_acc, faces_acc}

            true ->
              {:queue.in(next_t, q_acc), MapSet.put(seen_acc, next_t), faces_acc}
          end
        end)
        |> then(fn {nq, nseen, nfaces} -> bfs_helper(nq, nseen, cubes, neighbors, min, max, nfaces) end)

      {:empty, _} ->
        faces
    end
  end
    defp out_of_bounds({x,y,z}, {min_x, min_y, min_z}, {max_x, max_y, max_z}) do
        x < min_x or y < min_y or z < min_z or x > max_x or y > max_y or z > max_z
    end
end

Solution.run()
