
defmodule Solution do
  def solve(grid, parts) do
    parts_grid =
      parts
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {part, index}, acc ->
        (part.xmin..part.xmax)
        |> Enum.reduce(acc, fn x, acc2 ->
          Map.put(acc2, {x, part.y}, index)
        end)
      end)

    grid
    |> Enum.filter(fn {{_, _}, char} -> char == ?* end)
    |> Enum.reduce(0, fn {{x, y}, _}, acc ->
      neighbor_parts =
        [
          {0, 1},
          {0, -1},
          {1, 0},
          {-1, 0},
          {-1, -1},
          {-1, 1},
          {1, -1},
          {1, 1}
        ]
        |> Enum.reduce(MapSet.new(), fn {dx, dy}, set ->
          case Map.get(parts_grid, {x + dx, y + dy}) do
            nil -> set
            part_index -> MapSet.put(set, part_index)
          end
        end)

      if MapSet.size(neighbor_parts) == 2 do
        prod =
          neighbor_parts
          |> Enum.map(&Enum.at(parts, &1).n)
          |> Enum.reduce(1, &*/2)

        acc + prod
      else
        acc
      end
    end)
  end
end

input = File.read!("input.txt") |> String.trim()

lines = String.split(input, "\n")

grid =
  lines
  |> Enum.with_index()
  |> Enum.reduce(%{}, fn {line, y}, acc ->
    line
    |> String.to_charlist()
    |> Enum.with_index()
    |> Enum.reduce(acc, fn {char, x}, acc2 ->
      Map.put(acc2, {x, y}, char)
    end)
  end)

parts =
  lines
  |> Enum.with_index()
  |> Enum.reduce([], fn {line, y}, acc ->
    Regex.scan(~r/\d+/, line, return: :index)
    |> Enum.reduce(acc, fn [{start, len}], acc2 ->
      xmin = start
      xmax = start + len - 1
      n = String.slice(line, start, len) |> String.to_integer()
      acc2 ++ [%{xmin: xmin, xmax: xmax, y: y, n: n}]
    end)
  end)

Solution.solve(grid, parts) |> IO.puts()
