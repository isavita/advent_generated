
defmodule AdventOfCode2015.Day1 do
  def call do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
    |> Enum.reduce(0, &floor_instructions/2)
  end

  defp floor_instructions([], current_floor), do: current_floor

  defp floor_instructions([?\( | tail], current_floor), do: floor_instructions(tail, current_floor + 1)

  defp floor_instructions([?\) | tail], current_floor), do: floor_instructions(tail, current_floor - 1)
end