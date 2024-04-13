defmodule Assembler do
  def run do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
    |> execute_instructions(%{"a" => 0, "b" => 0}, 0)
    |> Map.get("b")
    |> IO.puts()
  end

  defp execute_instructions(instructions, registers, i) when i < length(instructions) do
    {instruction, registers, offset} = parse_instruction(Enum.at(instructions, i), registers)
    execute_instructions(instructions, registers, i + offset)
  end

  defp execute_instructions(_, registers, _), do: registers

  defp parse_instruction(instruction, registers) do
    [op | args] = String.split(instruction)
    case op do
      "hlf" -> {op, Map.update!(registers, hd(args), &div(&1, 2)), 1}
      "tpl" -> {op, Map.update!(registers, hd(args), &(&1 * 3)), 1}
      "inc" -> {op, Map.update!(registers, hd(args), &(&1 + 1)), 1}
      "jmp" -> {op, registers, String.to_integer(hd(args))}
      "jie" -> 
        if rem(Map.get(registers, String.slice(hd(args), 0, 1)), 2) == 0 do
          {op, registers, String.to_integer(List.last(args))}
        else
          {op, registers, 1}
        end
      "jio" -> 
        if Map.get(registers, String.slice(hd(args), 0, 1)) == 1 do
          {op, registers, String.to_integer(List.last(args))}
        else
          {op, registers, 1}
        end
    end
  end
end

Assembler.run()