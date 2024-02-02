
defmodule Submarine do
  def call do
    {horizontal_position, depth} = File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.reduce({0, 0}, fn command, {h_pos, d} ->
      [action, value] = String.split(command, " ")
      case action do
        "forward" -> {h_pos + String.to_integer(value), d}
        "down" -> {h_pos, d + String.to_integer(value)}
        "up" -> {h_pos, d - String.to_integer(value)}
      end
    end)
    
    horizontal_position * depth
  end
end
