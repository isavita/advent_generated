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
    Enum.reduce(instructions, {0, 0, :E}, fn {action, value}, {x, y, dir} ->
      case action do
        :N -> {x, y + value, dir}
        :S -> {x, y - value, dir}
        :E -> {x + value, y, dir}
        :W -> {x - value, y, dir}
        :L -> {x, y, turn(dir, -value)}
        :R -> {x, y, turn(dir, value)}
        :F -> move_forward(x, y, dir, value)
      end
    end)
  end

  defp turn(current_dir, degrees) do
    dirs = [:N, :E, :S, :W] # Clockwise order
    idx = Enum.find_index(dirs, &(&1 == current_dir))
    new_idx = rem(idx + div(degrees, 90), length(dirs))
    Enum.at(dirs, new_idx)
  end

  defp move_forward(x, y, :N, value), do: {x, y + value, :N}
  defp move_forward(x, y, :S, value), do: {x, y - value, :S}
  defp move_forward(x, y, :E, value), do: {x + value, y, :E}
  defp move_forward(x, y, :W, value), do: {x - value, y, :W}

  def call do
    instructions = read_input()
    {final_x, final_y, _} = process_instructions(instructions)
    manhattan_distance = abs(final_x) + abs(final_y)
    IO.puts("Manhattan distance: #{manhattan_distance}")
  end
end

Day12.call()
