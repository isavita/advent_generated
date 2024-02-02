
defmodule Submarine do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    {horizontal_position, depth, aim} = process_commands(input)
    horizontal_position * depth
  end

  def process_commands(commands) do
    {horizontal_position, depth, aim} = {0, 0, 0}
    Enum.reduce(commands, {horizontal_position, depth, aim}, fn command, {h, d, a} ->
      case String.split(command, " ") do
        ["forward", x] ->
          {h + String.to_integer(x), d + a * String.to_integer(x), a}
        ["down", x] ->
          {h, d, a + String.to_integer(x)}
        ["up", x] ->
          {h, d, a - String.to_integer(x)}
      end
    end)
  end
end
