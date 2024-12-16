
defmodule Solution do
  def solve do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, r} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _c} -> char == "0" end)
      |> Enum.map(fn {_char, c} -> {r, c} end)
    end)
    |> Enum.reduce(0, fn trailhead, acc ->
      acc + count_reachable(trailhead)
    end)
    |> IO.puts()
  end

  defp count_reachable(trailhead) do
    grid =
      File.read!("input.txt")
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&String.graphemes/1)
      |> Enum.map(fn row -> Enum.map(row, &String.to_integer/1) end)

    nr = length(grid)
    nc = length(Enum.at(grid, 0))
    dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]

    front = [{trailhead, 0}]
    visited = MapSet.new()
    reached = MapSet.new()

    count_reachable(grid, nr, nc, dirs, front, visited, reached)
  end

  defp count_reachable(_grid, _nr, _nc, _dirs, [], _visited, reached), do: MapSet.size(reached)

  defp count_reachable(grid, nr, nc, dirs, [{pos, h} | front], visited, reached) do
    if h == 9 do
      count_reachable(grid, nr, nc, dirs, front, visited, MapSet.put(reached, pos))
    else
      next_front =
        dirs
        |> Enum.reduce([], fn {dr, dc}, acc ->
          {nr2, nc2} = {elem(pos, 0) + dr, elem(pos, 1) + dc}
          if nr2 >= 0 and nr2 < nr and nc2 >= 0 and nc2 < nc do
            if Enum.at(Enum.at(grid, nr2), nc2) == h + 1 do
              key = {nr2, nc2, h + 1}
              if not MapSet.member?(visited, key) do
                [{ {nr2, nc2}, h + 1} | acc]
              else
                acc
              end
            else
              acc
            end
          else
            acc
          end
        end)
      count_reachable(grid, nr, nc, dirs, front ++ next_front, MapSet.union(visited, MapSet.new(Enum.map(next_front, fn {{r,c},h} -> {r,c,h} end))), reached)
    end
  end
end

Solution.solve()
