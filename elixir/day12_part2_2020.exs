defmodule Day12 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction(instruction) do
    [action, value_string] = Regex.run(~r/^([NSEWLRF])(\d+)$/, instruction, capture: :all_but_first)
    {String.to_atom(action), String.to_integer(value_string)}
  end

  defp process_instructions(instructions) do
    Enum.reduce(instructions, {{0, 0}, {10, 1}}, fn {action, value}, {{x, y}, {wx, wy}} ->
      case action do
        :N -> {{x, y}, {wx, wy + value}}
        :S -> {{x, y}, {wx, wy - value}}
        :E -> {{x, y}, {wx + value, wy}}
        :W -> {{x, y}, {wx - value, wy}}
        :L -> {{x, y}, rotate_waypoint({wx, wy}, -value)}
        :R -> {{x, y}, rotate_waypoint({wx, wy}, value)}
        :F -> {move_towards_waypoint({x, y}, {wx, wy}, value), {wx, wy}}
      end
    end)
  end

  defp rotate_waypoint({wx, wy}, degrees) do
    case rem(degrees, 360) do
      0 -> {wx, wy}
      90 -> {wy, -wx}
      180 -> {-wx, -wy}
      270 -> {-wy, wx}
      _ -> rotate_waypoint({wx, wy}, degrees + 360)
    end
  end

  defp move_towards_waypoint({x, y}, {wx, wy}, value) do
    {x + wx * value, y + wy * value}
  end

  def call do
    instructions = read_input()
    {{final_x, final_y}, _} = process_instructions(instructions)
    manhattan_distance = abs(final_x) + abs(final_y)
    IO.puts("Manhattan distance: #{manhattan_distance}")
  end
end

Day12.call()
