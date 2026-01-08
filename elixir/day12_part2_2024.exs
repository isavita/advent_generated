
defmodule Main do
  @moves [{"left", -1, 0}, {"up", 0, -1}, {"right", 1, 0}, {"down", 0, 1}]

  def main do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)
    h = length(lines)
    w = if h > 0, do: byte_size(Enum.at(lines, 0)), else: 0

    {total, _} = Enum.reduce(0..(h - 1), {0, MapSet.new()}, fn y, {sum, visited} ->
      {sum, visited} =
        Enum.reduce(0..(w - 1), {sum, visited}, fn x, {sum, visited} ->
          if :binary.at(Enum.at(lines, y), x) != ?. and not MapSet.member?(visited, {x, y}) do
            target = :binary.at(Enum.at(lines, y), x)
            {area, outer, visited} = flood(lines, h, w, {x, y}, target, visited)
            {sum + area * outer, visited}
          else
            {sum, visited}
          end
        end)

      {sum, visited}
    end)

    IO.puts(total)
  end

  defp flood(lines, h, w, {sx, sy}, target, visited_global) do
    stack = [{sx, sy, ""}]
    visited = MapSet.new()
    side = %{}
    {area, visited, side, _stack, visited_global} =
      flood_loop(lines, h, w, target, visited, side, stack, 0, visited_global)

    outer = count_outer(side)
    {area, outer, visited_global}
  end

  defp flood_loop(_lines, _h, _w, _target, visited, side, [], area, vg), do: {area, visited, side, [], vg}

  defp flood_loop(lines, h, w, target, visited, side, [{cx, cy, label} | rest], area, vg) do
    cond do
      cx < 0 or cx >= w or cy < 0 or cy >= h ->
        side = save_outer(label, side, cx, cy)
        flood_loop(lines, h, w, target, visited, side, rest, area, vg)

      true ->
        char = :binary.at(Enum.at(lines, cy), cx)

        if char != target do
          side =
            if label != "" and not MapSet.member?(visited, {cx, cy}) do
              save_outer(label, side, cx, cy)
            else
              side
            end

          flood_loop(lines, h, w, target, visited, side, rest, area, vg)
        else
          if MapSet.member?(visited, {cx, cy}) do
            flood_loop(lines, h, w, target, visited, side, rest, area, vg)
          else
            visited = MapSet.put(visited, {cx, cy})
            vg = MapSet.put(vg, {cx, cy})
            area = area + 1

            new_stack =
              Enum.reduce(@moves, rest, fn {lbl, dx, dy}, acc ->
                [{cx + dx, cy + dy, lbl} | acc]
              end)

            flood_loop(lines, h, w, target, visited, side, new_stack, area, vg)
          end
        end
    end
  end

  defp save_outer(label, side, x, y) do
    key = if label == "up" or label == "down", do: {y, x}, else: {x, y}
    Map.update(side, label, MapSet.new([key]), &MapSet.put(&1, key))
  end

  defp count_outer(side) do
    Enum.reduce(side, 0, fn {_label, set}, acc ->
      sorted = Enum.sort(set)

      {outer, _} =
        Enum.reduce(sorted, {0, MapSet.new()}, fn {i, j}, {c, temp} ->
          if check(temp, i, j) do
            {c, MapSet.put(temp, {i, j})}
          else
            {c + 1, MapSet.put(temp, {i, j})}
          end
        end)

      acc + outer
    end)
  end

  defp check(set, i, j) do
    MapSet.member?(set, {i, j - 1}) or MapSet.member?(set, {i, j + 1})
  end
end

Main.main()
