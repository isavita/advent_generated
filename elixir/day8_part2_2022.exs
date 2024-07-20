
defmodule MaxScore do
  def run do
    grid = read_input("input.txt")
    max_score = Enum.reduce(grid, 0, fn {point, height}, acc ->
      score = calculate_score(point, height, grid)
      max(acc, score)
    end)
    IO.puts(max_score)
  end

  defp read_input(file) do
    File.stream!(file)
    |> Stream.with_index()
    |> Enum.flat_map(fn {line, y} ->
      String.trim(line)
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.map(fn {b, x} -> {{x, y}, b - ?0} end)
    end)
    |> Enum.into(%{})
  end

  defp calculate_score({x, y}, height, grid) do
    directions = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
    Enum.reduce(directions, 1, fn {dx, dy}, acc ->
      view = count_view({x, y}, {dx, dy}, height, grid, 0)
      acc * view
    end)
  end

  defp count_view({x, y}, {dx, dy}, height, grid, view) do
    next = {x + dx, y + dy}
    case Map.get(grid, next) do
      nil -> view
      next_height when next_height < height -> count_view(next, {dx, dy}, height, grid, view + 1)
      _ -> view + 1
    end
  end
end

MaxScore.run()
