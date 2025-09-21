defmodule OpCode do
  defstruct name: nil, action: nil, a: nil, b: nil, match_count: []
end

defmodule Chronal do
  import Bitwise

  def parse_numbers(line) do
    Regex.scan(~r/-?\d+/, line)
    |> List.flatten()
    |> Enum.map(&String.to_integer/1)
  end

  def run_op(op, registers, instruction) do
    a = case op.a do
      :r -> Enum.at(registers, Enum.at(instruction, 1))
      :v -> Enum.at(instruction, 1)
    end
    b = case op.b do
      :r -> Enum.at(registers, Enum.at(instruction, 2))
      :v -> Enum.at(instruction, 2)
    end
    dest = Enum.at(instruction, 3)

    val = case op.action do
      :add -> a + b
      :mul -> a * b
      :and -> a &&& b
      :or  -> a ||| b
      :set -> a
      :gt  -> if a > b, do: 1, else: 0
      :eq  -> if a == b, do: 1, else: 0
    end

    List.replace_at(registers, dest, val)
  end

  def add_match(op, c) do
    if c in op.match_count do
      op
    else
      %{op | match_count: op.match_count ++ [c]}
    end
  end

  def remove_match(op, c) do
    %{op | match_count: List.delete(op.match_count, c)}
  end

  def test_code(registers, result, instruction, opcodes) do
    {opcodes_updated, sum} =
      Enum.map_reduce(opcodes, 0, fn opcode, acc ->
        new_regs = run_op(opcode, registers, instruction)
        if new_regs == result do
          {add_match(opcode, Enum.at(instruction, 0)), acc + 1}
        else
          {opcode, acc}
        end
      end)
    {sum, opcodes_updated}
  end

  def process_samples(lines, i, opcodes) do
    len = length(lines)
    if i >= len do
      {opcodes, i}
    else
      line = Enum.at(lines, i)
      if is_binary(line) and String.starts_with?(String.trim(line), "Before") do
        registers = parse_numbers(line)
        instruction = parse_numbers(Enum.at(lines, i + 1))
        result = parse_numbers(Enum.at(lines, i + 2))
        {sum, opcodes2} = test_code(registers, result, instruction, opcodes)
        process_samples(lines, i + 4, opcodes2)
      else
        {opcodes, i}
      end
    end
  end

  def deduce_opcodes(opcodes) do
    mapping = do_deduce(opcodes, %{})
    Enum.map(0..15, fn i -> Map.get(mapping, i) end)
  end

  def do_deduce(opcodes, mapping) do
    if map_size(mapping) == 16 do
      mapping
    else
      {ops2, mapping2} =
        Enum.reduce(opcodes, {opcodes, mapping}, fn opcode, {ops, map} ->
          if length(opcode.match_count) == 1 do
            c = List.first(opcode.match_count)
            if Map.has_key?(map, c) do
              {ops, map}
            else
              new_map = Map.put(map, c, opcode)
              new_ops = Enum.map(ops, fn o -> remove_match(o, c) end)
              {new_ops, new_map}
            end
          else
            {ops, map}
          end
        end)
      do_deduce(ops2, mapping2)
    end
  end

  def execute_program(lines, i, regs, final_opcodes, total_lines) do
    if i >= total_lines do
      regs
    else
      line = Enum.at(lines, i)
      trimmed = String.trim(line)
      if trimmed == "" do
        execute_program(lines, i + 1, regs, final_opcodes, total_lines)
      else
        nums = parse_numbers(line)
        op = Enum.at(final_opcodes, Enum.at(nums, 0))
        regs2 = run_op(op, regs, nums)
        execute_program(lines, i + 1, regs2, final_opcodes, total_lines)
      end
    end
  end

  def run do
    input = File.read!("input.txt") |> String.trim()
    lines = String.split(input, "\n", trim: false)

    opcodes = [
      %OpCode{name: "addr", action: :add, a: :r, b: :r},
      %OpCode{name: "addi", action: :add, a: :r, b: :v},
      %OpCode{name: "mulr", action: :mul, a: :r, b: :r},
      %OpCode{name: "muli", action: :mul, a: :r, b: :v},
      %OpCode{name: "banr", action: :and, a: :r, b: :r},
      %OpCode{name: "bani", action: :and, a: :r, b: :v},
      %OpCode{name: "borr", action: :or, a: :r, b: :r},
      %OpCode{name: "bori", action: :or, a: :r, b: :v},
      %OpCode{name: "setr", action: :set, a: :r, b: :r},
      %OpCode{name: "seti", action: :set, a: :v, b: :r},
      %OpCode{name: "gtir", action: :gt, a: :v, b: :r},
      %OpCode{name: "gtri", action: :gt, a: :r, b: :v},
      %OpCode{name: "gtrr", action: :gt, a: :r, b: :r},
      %OpCode{name: "eqir", action: :eq, a: :v, b: :r},
      %OpCode{name: "eqri", action: :eq, a: :r, b: :v},
      %OpCode{name: "eqrr", action: :eq, a: :r, b: :r}
    ]

    {opcodes_after, idx} = process_samples(lines, 0, opcodes)
    final_opcodes = deduce_opcodes(opcodes_after)
    start_prog = idx + 2
    regs = [0, 0, 0, 0]
    regs_final = execute_program(lines, start_prog, regs, final_opcodes, length(lines))
    IO.puts(Enum.at(regs_final, 0))
  end

  def main do
    run()
  end
end

defmodule Main do
  def main do
    Chronal.main()
  end
end

Main.main()