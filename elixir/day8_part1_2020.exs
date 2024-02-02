
defmodule Day8 do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    execute_instructions(input, 0, 0, [])
  end

  defp execute_instructions(instructions, index, acc, visited) do
    if index in visited do
      acc
    else
      case Enum.at(instructions, index) do
        "nop " <> _arg -> execute_instructions(instructions, index + 1, acc, [index | visited])
        "acc " <> arg ->
          acc_increment = String.to_integer(arg)
          execute_instructions(instructions, index + 1, acc + acc_increment, [index | visited])
        "jmp " <> arg ->
          jump = String.to_integer(arg)
          execute_instructions(instructions, index + jump, acc, [index | visited])
      end
    end
  end
end
