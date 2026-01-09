
defmodule Solver do
  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        grid = parse(content)
        {min_y, max_y} = grid |> Map.keys() |> Enum.map(&elem(&1, 1)) |> Enum.min_max()
        {final_grid, _} = fill(500, 0, grid, max_y)
        final_grid
        |> Enum.count(fn {{_, y}, v} -> y >= min_y and y <= max_y and v == ?~ end)
        |> IO.puts()
      _ -> :ok
    end
  end

  defp parse(content) do
    content
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, acc ->
      [[_, a, v, _, v1, v2]] = Regex.scan(~r/([xy])=(\d+), ([xy])=(\d+)\.\.(\d+)/, line)
      {v, v1, v2} = {String.to_integer(v), String.to_integer(v1), String.to_integer(v2)}
      if a == "x", do: Enum.reduce(v1..v2, acc, fn y, acc -> Map.put(acc, {v, y}, ?#) end),
                 else: Enum.reduce(v1..v2, acc, fn x, acc -> Map.put(acc, {x, v}, ?#) end)
    end)
  end

  defp fill(x, y, grid, max_y) do
    res = Map.get(grid, {x, y})
    cond do
      y > max_y -> {grid, false}
      res == ?# or res == ?~ -> {grid, true}
      res == ?| and Map.get(grid, {x, y + 1}) not in [?#, ?~] and Map.has_key?(grid, {x, y + 1}) -> {grid, false}
      true ->
        {grid, b_blkd} = fill(x, y + 1, Map.put(grid, {x, y}, ?|), max_y)
        if b_blkd do
          {grid, lx, lb} = spread(x - 1, y, -1, grid, max_y)
          {grid, rx, rb} = spread(x + 1, y, 1, grid, max_y)
          if lb and rb do
            {Enum.reduce(lx..rx, grid, fn ix, acc -> Map.put(acc, {ix, y}, ?~) end), true}
          else
            {Enum.reduce(lx..rx, grid, fn ix, acc -> Map.put(acc, {ix, y}, ?|) end), false}
          end
        else
          {grid, false}
        end
    end
  end

  defp spread(x, y, dx, grid, max_y) do
    res = Map.get(grid, {x, y})
    if res == ?# or res == ?~ do
      {grid, x - dx, true}
    else
      {grid, b} = fill(x, y + 1, Map.put(grid, {x, y}, ?|), max_y)
      if b, do: spread(x + dx, y, dx, grid, max_y), else: {grid, x, false}
    end
  end
end

Solver.main()
