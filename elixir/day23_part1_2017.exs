defmodule Day23 do
  def call do
    instructions = read_input("input.txt")
    mul_count = execute_instructions(instructions)
    IO.puts("The 'mul' instruction was invoked #{mul_count} times.")
  end

  defp read_input(file_path) do
    File.stream!(file_path)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.split(&1, " ", trim: true))
  end

  defp execute_instructions(instructions) do
    execute(instructions, %{}, 0, 0)
  end

  defp execute([], _registers, _pointer, mul_count), do: mul_count

  defp execute(instructions, registers, pointer, mul_count) do
    if pointer < 0 or pointer >= length(instructions), do: mul_count, else: process_instruction(Enum.at(instructions, pointer), registers, pointer, mul_count, instructions)
  end

  defp process_instruction(["set", x, y], registers, pointer, mul_count, instructions) do
    new_registers = Map.put(registers, x, get_value(y, registers))
    execute(instructions, new_registers, pointer + 1, mul_count)
  end

  defp process_instruction(["sub", x, y], registers, pointer, mul_count, instructions) do
    new_registers = Map.update(registers, x, get_value(y, registers), fn old -> old - get_value(y, registers) end)
    execute(instructions, new_registers, pointer + 1, mul_count)
  end

  defp process_instruction(["mul", x, y], registers, pointer, mul_count, instructions) do
    new_registers = Map.update(registers, x, get_value(y, registers), fn old -> old * get_value(y, registers) end)
    execute(instructions, new_registers, pointer + 1, mul_count + 1)
  end

  defp process_instruction(["jnz", x, y], registers, pointer, mul_count, instructions) do
    if get_value(x, registers) != 0 do
      execute(instructions, registers, pointer + get_value(y, registers), mul_count)
    else
      execute(instructions, registers, pointer + 1, mul_count)
    end
  end

  defp get_value(val, registers) do
    case Integer.parse(val) do
      {num, _} -> num
      :error -> Map.get(registers, val, 0)
    end
  end
end

Day23.call()
