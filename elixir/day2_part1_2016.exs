
defmodule BathroomSecurity do
  def call do
    input = File.read!("input.txt")
    instructions = String.split(input, "\n")

    keypad = [
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9]
    ]

    current_position = {1, 1} # Starting at button 5

    code = instructions
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(fn instruction ->
      Enum.reduce(instruction, current_position, fn move, {row, col} ->
        new_position =
          case move do
            "U" -> {max(row - 1, 0), col}
            "D" -> {min(row + 1, 2), col}
            "L" -> {row, max(col - 1, 0)}
            "R" -> {row, min(col + 1, 2)}
          end

        if Enum.at(Enum.at(keypad, elem(new_position, 0)), elem(new_position, 1)) != nil do
          new_position
        else
          {row, col}
        end
      end)
    end)
    |> Enum.map(fn {row, col} -> Enum.at(Enum.at(keypad, row), col) end)
    |> Enum.map(&Integer.to_string/1)
    |> Enum.join("")

    code
  end
end
