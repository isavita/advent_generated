
defmodule BootCode do
  def solve do
    instructions = File.read!("input.txt") |> String.split("\n", trim: true)

    Enum.find_value(0..(length(instructions) - 1), fn i ->
      modified_instructions = modify_instruction(instructions, i)
      case execute_boot_code(modified_instructions) do
        {:terminated, accumulator} -> accumulator
        _ -> nil
      end
    end)
    |> IO.puts()
  end

  defp modify_instruction(instructions, index) do
    List.update_at(instructions, index, fn instruction ->
      [op, arg] = String.split(instruction)
      case op do
        "jmp" -> "nop #{arg}"
        "nop" -> "jmp #{arg}"
        _ -> instruction
      end
    end)
  end

  defp execute_boot_code(instructions) do
    do_execute_boot_code(instructions, 0, 0, MapSet.new())
  end

  defp do_execute_boot_code(instructions, current_instruction, accumulator, visited) do
    cond do
      current_instruction >= length(instructions) ->
        {:terminated, accumulator}

      MapSet.member?(visited, current_instruction) ->
        {:loop, accumulator}

      true ->
        [op, arg] = instructions |> Enum.at(current_instruction) |> String.split()
        arg = String.to_integer(arg)

        case op do
          "acc" ->
            do_execute_boot_code(
              instructions, 
              current_instruction + 1, 
              accumulator + arg, 
              MapSet.put(visited, current_instruction)
            )
          "jmp" ->
            do_execute_boot_code(
              instructions, 
              current_instruction + arg, 
              accumulator, 
              MapSet.put(visited, current_instruction)
            )
          "nop" ->
            do_execute_boot_code(
              instructions, 
              current_instruction + 1, 
              accumulator, 
              MapSet.put(visited, current_instruction)
            )
        end
    end
  end
end

BootCode.solve()
