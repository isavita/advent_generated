
defmodule Day7 do
  def parse_instruction(instruction) do
    case String.split(instruction, " -> ") do
      [source, dest] -> {dest, parse_source(source)}
    end
  end

  defp parse_source(source) do
    cond do
      Regex.match?(~r/^\d+$/, source) ->
        {:value, String.to_integer(source)}

      Regex.match?(~r/^[a-z]+$/, source) ->
        {:wire, source}

      Regex.match?(~r/^NOT [a-z]+$/, source) ->
        [_, operand] = String.split(source)
        {:not, operand}

      Regex.match?(~r/^[a-z]+ AND [a-z]+$/, source) || Regex.match?(~r/^\d+ AND [a-z]+$/, source) || Regex.match?(~r/^[a-z]+ AND \d+$/, source) ->
        [op1, "AND", op2] = String.split(source)
        {:and, parse_operand(op1), parse_operand(op2)}

      Regex.match?(~r/^[a-z]+ OR [a-z]+$/, source) ->
        [op1, "OR", op2] = String.split(source)
        {:or, parse_operand(op1), parse_operand(op2)}

      Regex.match?(~r/^[a-z]+ LSHIFT \d+$/, source) ->
        [op, "LSHIFT", shift] = String.split(source)
        {:lshift, op, String.to_integer(shift)}

      Regex.match?(~r/^[a-z]+ RSHIFT \d+$/, source) ->
        [op, "RSHIFT", shift] = String.split(source)
        {:rshift, op, String.to_integer(shift)}
    end
  end

  defp parse_operand(operand) do
    if Regex.match?(~r/^\d+$/, operand) do
      {:value, String.to_integer(operand)}
    else
      {:wire, operand}
    end
  end


  def evaluate(instructions, wire, signals \\ %{}) do
    cond do
      Map.has_key?(signals, wire) ->
        {signals[wire], signals}

      true ->
        case instructions[wire] do
          {:value, val} ->
            {val, Map.put(signals, wire, val)}

          {:wire, source_wire} ->
            {val, updated_signals} = evaluate(instructions, source_wire, signals)
            {val, Map.put(updated_signals, wire, val)}

          {:not, operand} ->
              {op_val, updated_signals} = evaluate(instructions, operand, signals)
              result = 65535 - op_val
              {result, Map.put(updated_signals, wire, result)}

          {:and, op1, op2} ->
            {val1, signals1} = get_operand_value(instructions, signals, op1)
            {val2, signals2} = get_operand_value(instructions, signals1, op2)
            result = Bitwise.band(val1, val2)
            {result, Map.put(signals2, wire, result)}

          {:or, op1, op2} ->
            {val1, signals1} = get_operand_value(instructions, signals, op1)
            {val2, signals2} = get_operand_value(instructions, signals1, op2)
            result = Bitwise.bor(val1, val2)
            {result, Map.put(signals2, wire, result)}

          {:lshift, op, shift} ->
              {op_val, updated_signals} = evaluate(instructions, op, signals)
              result = Bitwise.bsl(op_val, shift)
              {result, Map.put(updated_signals, wire, result)}

          {:rshift, op, shift} ->
              {op_val, updated_signals} = evaluate(instructions, op, signals)
              result = Bitwise.bsr(op_val, shift)
              {result, Map.put(updated_signals, wire, result)}
        end
    end
  end

  defp get_operand_value(instructions, signals, operand) do
      case operand do
        {:value, val} -> {val, signals}
        {:wire, wire} -> evaluate(instructions, wire, signals)
      end
  end


  def solve() do
    instructions =
      File.read!("input.txt")
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&parse_instruction/1)
      |> Enum.into(%{})

    {result, _} = evaluate(instructions, "a")
    IO.puts(result)
  end
end

Day7.solve()
