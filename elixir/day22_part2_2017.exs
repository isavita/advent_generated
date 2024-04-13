defmodule VirusSimulation do
  def run do
    {grid, start_x, start_y} = read_input("input.txt")
    simulate(grid, start_x, start_y)
  end

  defp read_input(filename) do
    {grid, max_x, max_y} =
      File.stream!(filename)
      |> Enum.with_index()
      |> Enum.reduce({%{}, 0, 0}, fn {line, y}, {grid, max_x, _max_y} ->
        current_max_x = max(max_x, String.length(line) - 1)
        {update_grid(grid, line, y), current_max_x, y}
      end)

    start_x = div(max_x, 2)
    start_y = div(max_y, 2)
    {grid, start_x, start_y}
  end

  defp update_grid(grid, line, y) do
    line
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.reduce(grid, fn {char, x}, acc ->
      if char == "#" do
        Map.put(acc, {x, y}, 2) # Infected
      else
        acc
      end
    end)
  end

  defp simulate(grid, start_x, start_y) do
    directions = [{0, -1}, {1, 0}, {0, 1}, {-1, 0}]
    {infected_count, _} =
      Enum.reduce(0..9999999, {0, {start_x, start_y, 0, grid}}, fn _, {count, {x, y, dir, grid}} ->
        status = Map.get(grid, {x, y}, 0)
        {new_dir, new_status} = case status do
          0 -> {(dir - 1) |> rem(4), 1} # Weakened
          1 -> {dir, 2} # Infected
          2 -> {(dir + 1) |> rem(4), 3} # Flagged
          3 -> {(dir + 2) |> rem(4), 0} # Clean
        end
        new_count = if new_status == 2, do: count + 1, else: count
        {dx, dy} = Enum.at(directions, new_dir)
        new_grid = Map.put(grid, {x, y}, new_status)
        {new_count, {x + dx, y + dy, new_dir, new_grid}}
      end)

    IO.puts(infected_count)
  end
end

VirusSimulation.run()