
defmodule ChronalCoordinates do
  def call do
    input = File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, ", "))
    |> Enum.map(fn [x, y] -> {String.to_integer(x), String.to_integer(y)} end)

    max_x = Enum.max_by(input, &elem(&1, 0)) |> elem(0)
    max_y = Enum.max_by(input, &elem(&1, 1)) |> elem(1)

    finite_areas = for x <- 0..max_x, y <- 0..max_y, reduce: %{} do
      acc ->
        closest = Enum.min_by(input, fn {cx, cy} -> abs(x - cx) + abs(y - cy) end)
        distance = Enum.map(input, fn {cx, cy} -> abs(x - cx) + abs(y - cy) end) |> Enum.sum()
        if distance < 10000, do: Map.update(acc, closest, 1, &(&1 + 1)), else: acc
    end

    safe_region_size = for x <- 0..max_x, y <- 0..max_y, reduce: 0 do
      acc ->
        total_distance = Enum.map(input, fn {cx, cy} -> abs(x - cx) + abs(y - cy) end) |> Enum.sum()
        if total_distance < 10000, do: acc + 1, else: acc
    end

    {Map.values(finite_areas) |> Enum.max(), safe_region_size}
  end
end
