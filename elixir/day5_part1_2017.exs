
defmodule MazeSolver do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> solve(0, 0)
  end

  defp solve(instructions, position, steps) when position < 0 or position >= length(instructions), do: steps
  defp solve(instructions, position, steps) do
    jump = Enum.at(instructions, position)
    updated_instructions = List.replace_at(instructions, position, jump + 1)
    solve(updated_instructions, position + jump, steps + 1)
  end
end
