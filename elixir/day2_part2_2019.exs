defmodule Day2 do
  def read_input do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp process_intcode(program), do: process_intcode(program, 0)

  defp process_intcode(program, pointer) do
    case Enum.slice(program, pointer, 4) do
      [99 | _] ->
        program

      [opcode, pos_a, pos_b, pos_out] ->
        new_program =
          program
          |> execute(opcode, pos_a, pos_b, pos_out)

        process_intcode(new_program, pointer + 4)

      _ ->
        program
    end
  end

  defp execute(program, 1, pos_a, pos_b, pos_out) do
    result = Enum.at(program, pos_a) + Enum.at(program, pos_b)
    List.replace_at(program, pos_out, result)
  end

  defp execute(program, 2, pos_a, pos_b, pos_out) do
    result = Enum.at(program, pos_a) * Enum.at(program, pos_b)
    List.replace_at(program, pos_out, result)
  end

  defp try_combinations(program, noun, verb) do
    modified_program = List.replace_at(List.replace_at(program, 1, noun), 2, verb)
    [head | _] = process_intcode(modified_program)
    head
  end

  defp find_noun_verb(program, output) do
    try do
      for noun <- 0..99, verb <- 0..99 do
        result = try_combinations(program, noun, verb)
        if result == output, do: throw({noun, verb})
      end
    catch
      :throw, {noun, verb} -> {noun, verb}
    end
  end

  def call do
    program = read_input()
    {noun, verb} = find_noun_verb(program, 19_690_720)
    IO.puts(100 * noun + verb)
  end
end

Day2.call()
