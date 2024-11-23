
defmodule IntcodeComputer do
  def run_program(input_file) do
    input_file
    |> File.read!()
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> execute_program([1])
  end

  defp execute_program(memory, input) do
    execute_program(memory, input, 0)
  end

  defp execute_program(memory, input, ip) do
    {opcode, modes} = parse_instruction(Enum.at(memory, ip))

    case opcode do
      1 ->
        {memory, ip} = execute_add(memory, modes, ip)
        execute_program(memory, input, ip)

      2 ->
        {memory, ip} = execute_multiply(memory, modes, ip)
        execute_program(memory, input, ip)

      3 ->
        {memory, ip} = execute_input(memory, modes, ip, hd(input))
        execute_program(memory, tl(input), ip)

      4 ->
        {value, ip} = execute_output(memory, modes, ip)
        IO.puts(value)
        execute_program(memory, input, ip)

      99 ->
        :halt
    end
  end

  defp parse_instruction(instruction) do
    opcode = rem(instruction, 100)
    modes = div(instruction, 100) |> Integer.digits() |> Enum.reverse()
    {opcode, modes}
  end

  defp execute_add(memory, modes, ip) do
    [a, b, c] = Enum.slice(memory, ip + 1, 3)
    a = get_value(memory, a, Enum.at(modes, 0, 0))
    b = get_value(memory, b, Enum.at(modes, 1, 0))
    memory = List.replace_at(memory, c, a + b)
    {memory, ip + 4}
  end

  defp execute_multiply(memory, modes, ip) do
    [a, b, c] = Enum.slice(memory, ip + 1, 3)
    a = get_value(memory, a, Enum.at(modes, 0, 0))
    b = get_value(memory, b, Enum.at(modes, 1, 0))
    memory = List.replace_at(memory, c, a * b)
    {memory, ip + 4}
  end

  defp execute_input(memory, modes, ip, input) do
    [a] = Enum.slice(memory, ip + 1, 1)
    memory = List.replace_at(memory, a, input)
    {memory, ip + 2}
  end

  defp execute_output(memory, modes, ip) do
    [a] = Enum.slice(memory, ip + 1, 1)
    value = get_value(memory, a, Enum.at(modes, 0, 0))
    {value, ip + 2}
  end

  defp get_value(memory, param, mode) do
    case mode do
      0 -> Enum.at(memory, param)
      1 -> param
    end
  end
end

IntcodeComputer.run_program("input.txt")
