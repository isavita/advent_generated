
defmodule Day1 do
  def call do
    input = File.read!("input.txt") |> String.trim()
    instructions = String.split(input, ", ") |> Enum.map(&String.trim/1)

    {_, _, x, y} = Enum.reduce(instructions, {0, 1, 0, 0}, fn instruction, {dx, dy, x, y} ->
      turn = String.at(instruction, 0)
      distance = String.slice(instruction, 1..-1) |> String.to_integer()

      new_dx = if turn == "R" do dy else -dy end
      new_dy = if turn == "R" do -dx else dx end

      new_x = x + new_dx * distance
      new_y = y + new_dy * distance

      {new_dx, new_dy, new_x, new_y}
    end)

    abs(x) + abs(y)
  end
end
