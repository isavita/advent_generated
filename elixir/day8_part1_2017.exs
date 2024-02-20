defmodule Day8 do
  def call() do
    input = File.read!("input.txt")
    instructions = parse_instructions(input)
    final_registers = Enum.reduce(instructions, %{}, &execute_instruction/2)
    largest_value = final_registers |> Map.values() |> Enum.max()
    IO.puts(largest_value)
  end

  defp parse_instructions(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction(line) do
    [reg, op, val, "if", cond_reg, cond_op, cond_val] = String.split(line, " ")
    {reg, op, String.to_integer(val), cond_reg, cond_op, String.to_integer(cond_val)}
  end

  defp execute_instruction({reg, op, val, cond_reg, cond_op, cond_val}, registers) do
    cond_value = Map.get(registers, cond_reg, 0)
    if evaluate_condition(cond_value, cond_op, cond_val) do
      update_register(registers, reg, op, val)
    else
      registers
    end
  end

  defp evaluate_condition(value, op, cond_val) do
    case op do
      "==" -> value == cond_val
      "!=" -> value != cond_val
      ">" -> value > cond_val
      "<" -> value < cond_val
      ">=" -> value >= cond_val
      "<=" -> value <= cond_val
      _ -> false
    end
  end

  defp update_register(registers, reg, op, val) do
    current_val = Map.get(registers, reg, 0)
    new_val =
      case op do
        "inc" -> current_val + val
        "dec" -> current_val - val
        _ -> current_val
      end
    Map.put(registers, reg, new_val)
  end
end

Day8.call()
