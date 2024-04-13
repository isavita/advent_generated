defmodule Asteroids do
  def main do
    "input.txt"
    |> read_asteroids()
    |> find_best_asteroid_location()
    |> IO.inspect()
  end

  defp read_asteroids(filename) do
    File.stream!(filename)
    |> Enum.map(fn line ->
      for char <- String.graphemes(line), do: char == "#"
    end)
  end

  defp find_best_asteroid_location(asteroids) do
    asteroids
    |> Enum.with_index()
    |> Enum.reduce(0, fn {row, y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {is_asteroid, x}, inner_acc ->
        if is_asteroid do
          count = count_visible_asteroids(asteroids, x, y)
          max(inner_acc, count)
        else
          inner_acc
        end
      end)
    end)
  end

  defp count_visible_asteroids(asteroids, x, y) do
    asteroids
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {row, other_y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {is_asteroid, other_x}, inner_acc ->
        if is_asteroid && !(other_x == x && other_y == y) do
          angle = :math.atan2(other_y - y, other_x - x)
          Map.put(inner_acc, angle, true)
        else
          inner_acc
        end
      end)
    end)
    |> Map.keys()
    |> length()
  end
end

Asteroids.main()