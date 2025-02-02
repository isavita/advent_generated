
defmodule IntcodeComputer do
  def run(program, input) do
    do_run(program, 0, 0, input, [])
  end

  defp do_run(program, pc, relative_base, input, output) do
    opcode = program[pc] |> rem(100)
    mode1 = div(program[pc], 100) |> rem(10)
    mode2 = div(program[pc], 1000) |> rem(10)
    mode3 = div(program[pc], 10000) |> rem(10)

    case opcode do
      1 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        p2 = get_value(program, pc + 2, mode2, relative_base)
        p3 = get_address(program, pc + 3, mode3, relative_base)
        do_run(Map.put(program, p3, p1 + p2), pc + 4, relative_base, input, output)

      2 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        p2 = get_value(program, pc + 2, mode2, relative_base)
        p3 = get_address(program, pc + 3, mode3, relative_base)
        do_run(Map.put(program, p3, p1 * p2), pc + 4, relative_base, input, output)

      3 ->
        [h | t] = input
        p1 = get_address(program, pc + 1, mode1, relative_base)
        do_run(Map.put(program, p1, h), pc + 2, relative_base, t, output)

      4 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        do_run(program, pc + 2, relative_base, input, [p1 | output])

      5 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        p2 = get_value(program, pc + 2, mode2, relative_base)

        if p1 != 0 do
          do_run(program, p2, relative_base, input, output)
        else
          do_run(program, pc + 3, relative_base, input, output)
        end

      6 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        p2 = get_value(program, pc + 2, mode2, relative_base)

        if p1 == 0 do
          do_run(program, p2, relative_base, input, output)
        else
          do_run(program, pc + 3, relative_base, input, output)
        end

      7 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        p2 = get_value(program, pc + 2, mode2, relative_base)
        p3 = get_address(program, pc + 3, mode3, relative_base)

        if p1 < p2 do
          do_run(Map.put(program, p3, 1), pc + 4, relative_base, input, output)
        else
          do_run(Map.put(program, p3, 0), pc + 4, relative_base, input, output)
        end

      8 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        p2 = get_value(program, pc + 2, mode2, relative_base)
        p3 = get_address(program, pc + 3, mode3, relative_base)

        if p1 == p2 do
          do_run(Map.put(program, p3, 1), pc + 4, relative_base, input, output)
        else
          do_run(Map.put(program, p3, 0), pc + 4, relative_base, input, output)
        end

      9 ->
        p1 = get_value(program, pc + 1, mode1, relative_base)
        do_run(program, pc + 2, relative_base + p1, input, output)

      99 ->
        Enum.reverse(output)

      _ ->
        raise "Unknown opcode: #{opcode}"
    end
  end

  defp get_value(program, address, mode, relative_base) do
    case mode do
      0 -> program[program[address] || 0] || 0
      1 -> program[address] || 0
      2 -> program[(program[address] || 0) + relative_base] || 0
    end
  end

  defp get_address(program, address, mode, relative_base) do
    case mode do
      0 -> program[address] || 0
      2 -> (program[address] || 0) + relative_base
      _ -> raise "Invalid address mode for write operation: #{mode}"
    end
  end
end

input =
  File.read!("input.txt")
  |> String.trim()
  |> String.split(",")
  |> Enum.map(&String.to_integer/1)
  |> Enum.with_index()
  |> Map.new(fn {val, idx} -> {idx, val} end)

# Part 1
output1 = IntcodeComputer.run(input, [1])
IO.puts("BOOST keycode (Part 1): #{Enum.join(output1, ",")}")

# Part 2
output2 = IntcodeComputer.run(input, [2])
IO.puts("Distress signal coordinates (Part 2): #{Enum.join(output2, ",")}")
