
defmodule CrossedWires do
  def call do
    [wire1, wire2] = File.read!("input.txt") |> String.split("\n")
    {path1, path2} = {trace_path(wire1), trace_path(wire2)}
    intersections = MapSet.intersection(MapSet.new(path1), MapSet.new(path2))
    Enum.map(intersections, &manhattan_distance(&1)) |> Enum.min()
  end

  defp trace_path(wire) do
    wire
    |> String.split(",")
    |> Enum.reduce({{0, 0}, []}, fn instruction, {{x, y}, acc} ->
      {direction, steps} = parse_instruction(instruction)
      {new_coords, path} = apply_moves({x, y}, direction, String.to_integer(steps))
      {new_coords, acc ++ path}
    end)
    |> elem(1)
  end

  defp parse_instruction(instruction) do
    {String.slice(instruction, 0, 1), String.slice(instruction, 1..-1)}
  end

  defp apply_moves(coords, direction, steps) do
    Enum.reduce(1..steps, {coords, []}, fn _, {{x, y}, acc} ->
      new_coords = case direction do
        "U" -> {x, y + 1}
        "D" -> {x, y - 1}
        "L" -> {x - 1, y}
        "R" -> {x + 1, y}
      end
      {new_coords, acc ++ [new_coords]}
    end)
  end

  defp manhattan_distance({x, y}), do: abs(x) + abs(y)
end
