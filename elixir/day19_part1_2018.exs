
defmodule Solution do
  def solve do
    {:ok, content} = File.read("input.txt")
    lines = String.split(content, "\n", trim: true)

    [ip_bind_line | instruction_lines] = lines
    ip_bind = ip_bind_line |> String.split(" ") |> List.last() |> String.to_integer()

    instructions = 
      instruction_lines
      |> Enum.map(&String.split/1)

    run_program(instructions, ip_bind)
  end

  def run_program(instructions, ip_bind) do
    registers = List.duplicate(0, 6)
    do_run(instructions, ip_bind, registers, 0)
  end

  defp do_run(instructions, ip_bind, registers, ip) when ip >= 0 and ip < length(instructions) do
    registers = List.replace_at(registers, ip_bind, ip)
    
    [opcode | args] = Enum.at(instructions, ip)
    registers = execute_instruction(opcode, args, registers)
    
    new_ip = Enum.at(registers, ip_bind) + 1
    do_run(instructions, ip_bind, registers, new_ip)
  end

  defp do_run(instructions, _ip_bind, registers, _ip) do
    hd(registers)
  end

  defp execute_instruction(opcode, args, registers) do
    [a, b, c] = Enum.map(args, &String.to_integer/1)
    
    case opcode do
      "addr" -> List.replace_at(registers, c, Enum.at(registers, a) + Enum.at(registers, b))
      "addi" -> List.replace_at(registers, c, Enum.at(registers, a) + b)
      "mulr" -> List.replace_at(registers, c, Enum.at(registers, a) * Enum.at(registers, b))
      "muli" -> List.replace_at(registers, c, Enum.at(registers, a) * b)
      "banr" -> List.replace_at(registers, c, Bitwise.band(Enum.at(registers, a), Enum.at(registers, b)))
      "bani" -> List.replace_at(registers, c, Bitwise.band(Enum.at(registers, a), b))
      "borr" -> List.replace_at(registers, c, Bitwise.bor(Enum.at(registers, a), Enum.at(registers, b)))
      "bori" -> List.replace_at(registers, c, Bitwise.bor(Enum.at(registers, a), b))
      "setr" -> List.replace_at(registers, c, Enum.at(registers, a))
      "seti" -> List.replace_at(registers, c, a)
      "gtir" -> List.replace_at(registers, c, if(a > Enum.at(registers, b), do: 1, else: 0))
      "gtri" -> List.replace_at(registers, c, if(Enum.at(registers, a) > b, do: 1, else: 0))
      "gtrr" -> List.replace_at(registers, c, if(Enum.at(registers, a) > Enum.at(registers, b), do: 1, else: 0))
      "eqir" -> List.replace_at(registers, c, if(a == Enum.at(registers, b), do: 1, else: 0))
      "eqri" -> List.replace_at(registers, c, if(Enum.at(registers, a) == b, do: 1, else: 0))
      "eqrr" -> List.replace_at(registers, c, if(Enum.at(registers, a) == Enum.at(registers, b), do: 1, else: 0))
    end
  end
end

Solution.solve() |> IO.puts()
