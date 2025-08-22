
defmodule Main do
  @north {0, -1}
  @west {-1, 0}
  @south {0, 1}
  @east {1, 0}
  @empty ?. 
  @cubic ?#
  @round ?O
  @cycles 1_000_000_000

  def main do
    input =
      "input.txt"
      |> File.read!()
      |> String.trim()
      |> String.split("\n")

    {grid, w, h} = build_grid(input)
    result = solve(grid, w, h)
    IO.puts(result)
  end

  defp build_grid(lines) do
    w = String.length(List.first(lines))
    h = length(lines)

    data =
      lines
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {c, x}, a ->
          if c != @empty, do: Map.put(a, {x, y}, c), else: a
        end)
      end)

    {data, w, h}
  end

  defp in_bounds?({x, y}, w, h), do: x >= 0 and x < w and y >= 0 and y < h

  defp add({x1, y1}, {x2, y2}), do: {x1 + x2, y1 + y2}

  defp shift_single(grid, {x, y} = coord, dir, w, h) do
    case Map.get(grid, coord) do
      @round ->
        cur = coord
        nxt = add(coord, dir)

        cond do
          not in_bounds?(nxt, w, h) -> grid
          Map.has_key?(grid, nxt) -> grid
          true ->
            grid = Map.put(grid, nxt, @round) |> Map.delete(cur)
            shift_single(grid, nxt, dir, w, h)
        end

      _ -> grid
    end
  end

  defp shift_rocks(grid, dir, w, h) do
    xs = if dir in [@north, @west], do: 0..(w - 1), else: (w - 1)..0
    ys = if dir in [@north, @west], do: 0..(h - 1), else: (h - 1)..0

    Enum.reduce(xs, grid, fn x, g ->
      Enum.reduce(ys, g, fn y, gg ->
        shift_single(gg, {x, y}, dir, w, h)
      end)
    end)
  end

  defp cycle(grid, w, h) do
    grid
    |> shift_rocks(@north, w, h)
    |> shift_rocks(@west, w, h)
    |> shift_rocks(@south, w, h)
    |> shift_rocks(@east, w, h)
  end

  defp grid_key(grid, w) do
    grid
    |> Enum.reduce(0, fn {{x, y}, v}, acc ->
      if v == @round, do: acc + x + y * w, else: acc
    end)
  end

  defp load(grid, h) do
    grid
    |> Enum.reduce(0, fn {{_, y}, v}, acc ->
      if v == @round, do: acc + h - y, else: acc
    end)
  end

  defp repeat_cycle(grid, 0, _w, _h), do: grid
  defp repeat_cycle(grid, n, w, h), do: repeat_cycle(cycle(grid, w, h), n - 1, w, h)

  defp solve(grid, w, h) do
    loop(grid, %{}, 0, w, h)
  end

  defp loop(grid, cache, i, w, h) when i == @cycles do
    load(grid, h)
  end

  defp loop(grid, cache, i, w, h) do
    key = grid_key(grid, w)

    case Map.fetch(cache, key) do
      {:ok, start_i} ->
        period = i - start_i
        remaining = rem(@cycles - start_i, period)
        grid = repeat_cycle(grid, remaining, w, h)
        load(grid, h)

      :error ->
        cache = Map.put(cache, key, i)
        grid = cycle(grid, w, h)
        loop(grid, cache, i + 1, w, h)
    end
  end
end

Main.main()
