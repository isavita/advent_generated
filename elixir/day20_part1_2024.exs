
defmodule Main do
  defp read_input do
    grid =
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)

    h = length(grid)
    w = grid |> hd() |> length()

    {s, e} =
      for {row, x} <- Enum.with_index(grid),
          {cell, y} <- Enum.with_index(row),
          reduce: {nil, nil} do
        {s, e} ->
          case cell do
            "S" -> {{x, y}, e}
            "E" -> {s, {x, y}}
            _ -> {s, e}
          end
      end

    walls = for {row, x} <- Enum.with_index(grid), {cell, y} <- Enum.with_index(row), cell == "#", into: MapSet.new(), do: {x, y}
    track = for {row, x} <- Enum.with_index(grid), {cell, y} <- Enum.with_index(row), cell != "#", into: MapSet.new(), do: {x, y}

    {h, w, s, e, walls, track}
  end

  defp bfs(start, track, h, w) do
    queue = :queue.in({start, 0}, :queue.new())
    dist = Map.put(%{}, start, 0)

    {dist, _} =
      Stream.unfold({queue, dist}, fn
        {q, dist} ->
          case :queue.out(q) do
            {{:value, {{x, y}, d}}, q} ->
              neighbors =
                [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
                |> Enum.filter(fn {nx, ny} -> nx >= 0 and nx < h and ny >= 0 and ny < w end)
                |> Enum.filter(&MapSet.member?(track, &1))
                |> Enum.reject(&Map.has_key?(dist, &1))

              q = Enum.reduce(neighbors, q, &:queue.in({&1, d + 1}, &2))
              dist = Enum.reduce(neighbors, dist, &Map.put(&2, &1, d + 1))
              {{dist, {x, y}}, {q, dist}}

            {:empty, _} ->
              nil
          end
      end)
      |> Enum.reduce({%{}, nil}, fn {d, _}, _ -> {d, nil} end)

    dist
  end

  def main do
    {h, w, s, e, walls, track} = read_input()
    dist_s = bfs(s, track, h, w)
    dist_e = bfs(e, track, h, w)

    normal = dist_e[s]

    cheats =
      for {x, y} <- MapSet.to_list(track),
          d1 = Map.get(dist_s, {x, y}, -1),
          d1 != -1,
          {dx, dy} <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
          {mx, my} = {x + dx, y + dy},
          MapSet.member?(walls, {mx, my}),
          {dx2, dy2} <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
          {nx, ny} = {mx + dx2, my + dy2},
          MapSet.member?(track, {nx, ny}),
          d2 = Map.get(dist_e, {nx, ny}, -1),
          d2 != -1,
          normal - (d1 + 2 + d2) >= 100 do
        1
      end
      |> length()

    IO.puts(cheats)
  end
end

Main.main()
