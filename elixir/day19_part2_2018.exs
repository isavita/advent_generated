
defmodule Solution do
  def instructions do
    %{
      "addr" => fn r, a, b -> Enum.at(r, a) + Enum.at(r, b) end,
      "addi" => fn r, a, b -> Enum.at(r, a) + b end,
      "mulr" => fn r, a, b -> Enum.at(r, a) * Enum.at(r, b) end,
      "muli" => fn r, a, b -> Enum.at(r, a) * b end,
      "banr" => fn r, a, b -> Bitwise.band(Enum.at(r, a), Enum.at(r, b)) end,
      "bani" => fn r, a, b -> Bitwise.band(Enum.at(r, a), b) end,
      "borr" => fn r, a, b -> Bitwise.bor(Enum.at(r, a), Enum.at(r, b)) end,
      "bori" => fn r, a, b -> Bitwise.bor(Enum.at(r, a), b) end,
      "setr" => fn r, a, _ -> Enum.at(r, a) end,
      "seti" => fn _, a, _ -> a end,
      "gtir" => fn r, a, b -> if a > Enum.at(r, b), do: 1, else: 0 end,
      "gtri" => fn r, a, b -> if Enum.at(r, a) > b, do: 1, else: 0 end,
      "gtrr" => fn r, a, b -> if Enum.at(r, a) > Enum.at(r, b), do: 1, else: 0 end,
      "eqir" => fn r, a, b -> if a == Enum.at(r, b), do: 1, else: 0 end,
      "eqri" => fn r, a, b -> if Enum.at(r, a) == b, do: 1, else: 0 end,
      "eqrr" => fn r, a, b -> if Enum.at(r, a) == Enum.at(r, b), do: 1, else: 0 end
    }
  end

  def load_program(lines) do
    {ip_register, program} = 
      Enum.reduce(lines, {0, []}, fn line, {ip, prog} ->
        cond do
          String.starts_with?(line, "#ip") -> 
            {String.to_integer(String.split(line) |> Enum.at(1)), prog}
          true ->
            [op | nums] = String.split(line)
            [a, b, c] = Enum.map(nums, &String.to_integer/1)
            {ip, prog ++ [fn r -> 
              List.replace_at(r, c, instructions()[op].(r, a, b)) 
            end]}
        end
      end)
    {ip_register, program}
  end

  def run_program(ip_register, program, registers, max_cycles \\ 1000) do
    do_run(ip_register, program, registers, 0, 0, max_cycles)
  end

  defp do_run(ip_register, program, registers, ip, cycles, max_cycles) 
  when ip >= 0 and ip < length(program) and (max_cycles == 0 or cycles < max_cycles) do
    registers = List.replace_at(registers, ip_register, ip)
    registers = Enum.at(program, ip).(registers)
    ip = Enum.at(registers, ip_register) + 1
    do_run(ip_register, program, registers, ip, cycles + 1, max_cycles)
  end

  defp do_run(_, _, registers, _, _, _), do: registers

  def solve do
    {:ok, content} = File.read("input.txt")
    lines = String.split(content, "\n", trim: true)

    {ip_register, program} = load_program(lines)

    registers = List.replace_at(List.duplicate(0, 6), 0, 1)
    registers = run_program(ip_register, program, registers, 1000)
    
    n = Enum.max(registers)
    total = Enum.sum(for i <- 1..n, rem(n, i) == 0, do: i)
    
    IO.puts(total)
  end
end

Solution.solve()
