
defmodule HexEd do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.split(",")
    {final_distance, max_distance} = calculate_distance(input)
    {final_distance, max_distance}
  end

  defp calculate_distance(input) do
    {x, y} = {0, 0}
    max_distance = 0

    {final_x, final_y, max_distance} = Enum.reduce(input, {x, y, max_distance}, fn direction, {current_x, current_y, current_max} ->
      case direction do
        "n" -> {current_x, current_y + 1, max(current_max, distance_from_origin(current_x, current_y + 1))}
        "ne" -> {current_x + 1, current_y, max(current_max, distance_from_origin(current_x + 1, current_y))}
        "se" -> {current_x + 1, current_y - 1, max(current_max, distance_from_origin(current_x + 1, current_y - 1))}
        "s" -> {current_x, current_y - 1, max(current_max, distance_from_origin(current_x, current_y - 1))}
        "sw" -> {current_x - 1, current_y, max(current_max, distance_from_origin(current_x - 1, current_y))}
        "nw" -> {current_x - 1, current_y + 1, max(current_max, distance_from_origin(current_x - 1, current_y + 1))}
      end
    end)

    {distance_from_origin(final_x, final_y), max_distance}
  end

  defp distance_from_origin(x, y) do
    (abs(x) + abs(y) + abs(x + y)) / 2
  end
end
