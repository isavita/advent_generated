
defmodule Intcode do
  def call do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> restore_to_1202_program_alarm()
    |> process_program()
    |> hd()
  end

  defp restore_to_1202_program_alarm(program) do
    List.replace_at(program, 1, 12)
    |> List.replace_at(2, 2)
  end

  defp process_program(program), do: process_opcodes(program, 0)

  defp process_opcodes(program, index) do
    case Enum.slice(program, index, 4) do
      [99, _, _, _] -> program
      [1, a, b, c] -> process_opcodes(update_program(program, c, Enum.at(program, a) + Enum.at(program, b)), index + 4)
      [2, a, b, c] -> process_opcodes(update_program(program, c, Enum.at(program, a) * Enum.at(program, b)), index + 4)
      _ -> raise "Unknown opcode"
    end
  end

  defp update_program(program, index, value) do
    List.replace_at(program, index, value)
  end
end
