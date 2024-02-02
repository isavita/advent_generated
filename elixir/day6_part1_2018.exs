
defmodule ChronalCoordinates do
  def call do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, ", "))
    |> Enum.map(fn [x, y] -> {String.to_integer(x), String.to_integer(y)} end)
    |> largest_finite_area()
  end

  defp largest_finite_area(coords) do
    min_x = Enum.min_by(coords, &elem(&1, 0)) |> elem(0)
    max_x = Enum.max_by(coords, &elem(&1, 0)) |> elem(0)
    min_y = Enum.min_by(coords, &elem(&1, 1)) |> elem(1)
    max_y = Enum.max_by(coords, &elem(&1, 1)) |> elem(1)

    grid = for x <- min_x..max_x, y <- min_y..max_y, do: {x, y}

    closest_counts = Enum.reduce(grid, %{}, fn {x, y}, acc ->
      closest = Enum.min_by(coords, fn {cx, cy} -> abs(cx - x) + abs(cy - y) end)
      distance = abs(elem(closest, 0) - x) + abs(elem(closest, 1) - y)
      same_distance = Enum.filter(coords, fn {cx, cy} -> abs(cx - x) + abs(cy - y) == distance end)

      if length(same_distance) == 1 do
        Map.update(acc, closest, 1, &(&1 + 1))
      else
        acc
      end
    end)

    infinite_coords = Enum.reduce(grid, [], fn {x, y}, acc ->
      closest = Enum.min_by(coords, fn {cx, cy} -> abs(cx - x) + abs(cy - y) end)
      distance = abs(elem(closest, 0) - x) + abs(elem(closest, 1) - y)
      same_distance = Enum.filter(coords, fn {cx, cy} -> abs(cx - x) + abs(cy - y) == distance end)

      if x == min_x or x == max_x or y == min_y or y == max_y and length(same_distance) == 1 do
        [closest | acc]
      else
        acc
      end
    end)
    |> Enum.uniq()

    finite_counts = Map.drop(closest_counts, infinite_coords)

    Enum.max_by(finite_counts, fn {_k, v} -> v end) |> elem(1)
  end
end
