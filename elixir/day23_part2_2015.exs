
defmodule TuringLock do
  def run_program(start_a \\ 0) do
    instructions = read_instructions("input.txt")
    execute(instructions, %{a: start_a, b: 0}, 0)
  end

  defp read_instructions(file) do
    File.read!(file)
    |> String.split("\n", trim: true)
  end

  defp execute(instructions, registers, pointer) do
    case Enum.at(instructions, pointer) do
      nil -> registers.b  # Exit condition: no more instructions
      instruction -> 
        {new_registers, new_pointer} = process_instruction(instruction, registers, pointer)
        execute(instructions, new_registers, new_pointer)
    end
  end

  defp process_instruction("hlf a", registers, pointer) do
    {Map.update!(registers, :a, &div(&1, 2)), pointer + 1}
  end

  defp process_instruction("hlf b", registers, pointer) do
    {Map.update!(registers, :b, &div(&1, 2)), pointer + 1}
  end

  defp process_instruction("tpl a", registers, pointer) do
    {Map.update!(registers, :a, &(&1 * 3)), pointer + 1}
  end

  defp process_instruction("tpl b", registers, pointer) do
    {Map.update!(registers, :b, &(&1 * 3)), pointer + 1}
  end

  defp process_instruction("inc a", registers, pointer) do
    {Map.update!(registers, :a, &(&1 + 1)), pointer + 1}
  end

  defp process_instruction("inc b", registers, pointer) do
    {Map.update!(registers, :b, &(&1 + 1)), pointer + 1}
  end

  defp process_instruction("jmp " <> offset, registers, pointer) do
    {registers, pointer + String.to_integer(offset)}
  end

  defp process_instruction("jie " <> rest, registers, pointer) do
    [reg, offset] = String.split(rest, ", ")
    offset_value = String.to_integer(String.trim(offset))
    
    if rem(Map.get(registers, String.to_atom(reg)), 2) == 0 do
      {registers, pointer + offset_value}
    else
      {registers, pointer + 1}
    end
  end

  defp process_instruction("jio " <> rest, registers, pointer) do
    [reg, offset] = String.split(rest, ", ")
    offset_value = String.to_integer(String.trim(offset))
    
    if Map.get(registers, String.to_atom(reg)) == 1 do
      {registers, pointer + offset_value}
    else
      {registers, pointer + 1}
    end
  end
end

# To run the program and print the value of register b
IO.puts TuringLock.run_program(1)  # For Part Two
