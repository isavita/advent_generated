defmodule CrossedWires do
  def run do
    [wire1, wire2] = File.read!("input.txt")
                      |> String.trim()
                      |> String.split("\n")
                      |> Enum.map(&path_to_points/1)

    intersections = MapSet.intersection(MapSet.new(wire1), MapSet.new(wire2))

    steps_to_intersections = Enum.map(intersections, fn intersection ->
      steps(wire1, intersection) + steps(wire2, intersection)
    end)

    fewest_steps = Enum.min(steps_to_intersections)

    IO.puts("Fewest combined steps to reach an intersection: #{fewest_steps}")
  end

  defp path_to_points(path) do
    path
    |> String.split(",")
    |> Enum.reduce({{0, 0}, []}, fn direction, {current_pos, acc} ->
      steps = String.slice(direction, 1..-1) |> String.to_integer()
      Enum.reduce(1..steps, {current_pos, acc}, fn _, {{x, y}, acc} ->
        next_pos = case String.at(direction, 0) do
          "R" -> {x + 1, y}
          "L" -> {x - 1, y}
          "U" -> {x, y + 1}
          "D" -> {x, y - 1}
        end
        {next_pos, [next_pos | acc]}
      end)
    end)
    |> elem(1)
    |> Enum.reverse()
  end

  defp steps(path, intersection, step \\ 0)
  defp steps([point | _], point, step), do: step + 1
  defp steps([_ | rest], intersection, step), do: steps(rest, intersection, step + 1)

end

CrossedWires.run()
