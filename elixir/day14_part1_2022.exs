
defmodule Solution do
  def solve() do
    grid =
      "input.txt"
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Stream.map(&parse_line/1)
      |> Stream.flat_map(&points_between/1)
      |> Enum.into(MapSet.new())

    fill(grid)
  end

  defp parse_line(line) do
    line
    |> String.split(" -> ")
    |> Enum.map(&parse_point/1)
  end

  defp parse_point(point_str) do
    [x, y] =
      point_str
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    {x, y}
  end

  defp points_between([{x1, y1}, {x2, y2}]) do
    cond do
      x1 == x2 ->
        for y <- min(y1, y2)..max(y1, y2), do: {x1, y}

      y1 == y2 ->
        for x <- min(x1, x2)..max(x1, x2), do: {x, y1}
    end
  end

  defp points_between([head | [next | tail]]) do
    points_between([head, next]) ++ points_between([next | tail])
  end

  defp fill(grid) do
    floor = bounds(grid) + 1
    do_fill(grid, floor, 0, 0)
  end

  defp bounds(grid) do
    grid
    |> Enum.map(fn {_, y} -> y end)
    |> Enum.max()
  end

  defp do_fill(grid, floor, sands, first_floor_touch) do
    if MapSet.member?(grid, {500, 0}) do
      first_floor_touch
    else
      {new_grid, new_sands, new_first_floor_touch} = drop_sand(grid, floor, {500, 0}, sands, first_floor_touch)
      do_fill(new_grid, floor, new_sands, new_first_floor_touch)
    end
  end

  defp drop_sand(grid, floor, {x, y} = sand, sands, first_floor_touch) do
    cond do
      y == floor ->
        new_first_floor_touch = if first_floor_touch == 0, do: sands, else: first_floor_touch
        {MapSet.put(grid, sand), sands + 1, new_first_floor_touch}

      not MapSet.member?(grid, {x, y + 1}) ->
        drop_sand(grid, floor, {x, y + 1}, sands, first_floor_touch)

      not MapSet.member?(grid, {x - 1, y + 1}) ->
        drop_sand(grid, floor, {x - 1, y + 1}, sands, first_floor_touch)

      not MapSet.member?(grid, {x + 1, y + 1}) ->
        drop_sand(grid, floor, {x + 1, y + 1}, sands, first_floor_touch)

      true ->
        {MapSet.put(grid, sand), sands + 1, first_floor_touch}
    end
  end
end

Solution.solve()
|> IO.puts()
