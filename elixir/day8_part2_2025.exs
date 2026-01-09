
defmodule Solution do
  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        pts_list = content
          |> String.split("\n", trim: true)
          |> Enum.map(fn line ->
            line |> String.split(",") |> Enum.map(&(String.trim(&1) |> String.to_integer()))
          end)

        n = length(pts_list)
        if n >= 2, do: solve(pts_list, n)
      _ -> :ok
    end
  end

  defp solve(pts_list, n) do
    pts = List.to_tuple(pts_list)
    edges = for i <- 0..(n - 2), j <- (i + 1)..(n - 1) do
      [x1, y1, z1] = elem(pts, i)
      [x2, y2, z2] = elem(pts, j)
      dx = x1 - x2
      dy = y1 - y2
      dz = z1 - z2
      {dx * dx + dy * dy + dz * dz, i, j}
    end |> Enum.sort()

    parent = Map.new(0..(n - 1), &{&1, &1})
    rank = Map.new(0..(n - 1), &{&1, 0})

    Enum.reduce_while(edges, {parent, rank, n}, fn {_, u, v}, {p, r, comps} ->
      {ru, p} = find(p, u)
      {rv, p} = find(p, v)

      if ru != rv do
        if comps == 2 do
          [x1, y1, z1] = elem(pts, u)
          [x2, y2, z2] = elem(pts, v)
          IO.puts("Connected #{x1},#{y1},#{z1} and #{x2},#{y2},#{z2}")
          IO.puts("Product of X coordinates: #{x1 * x2}")
          {:halt, nil}
        else
          {p, r} = unite(p, r, ru, rv)
          {:cont, {p, r, comps - 1}}
        end
      else
        {:cont, {p, r, comps}}
      end
    end)
  end

  defp find(p, x) do
    if p[x] == x do
      {x, p}
    else
      {root, p} = find(p, p[x])
      {root, Map.put(p, x, root)}
    end
  end

  defp unite(p, r, ru, rv) do
    cond do
      r[ru] < r[rv] -> {Map.put(p, ru, rv), r}
      r[ru] > r[rv] -> {Map.put(p, rv, ru), r}
      true -> {Map.put(p, rv, ru), Map.put(r, ru, r[ru] + 1)}
    end
  end
end

Solution.main()
