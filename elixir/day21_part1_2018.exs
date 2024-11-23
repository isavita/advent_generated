
defmodule Solution do
  def solve(input) do
    {ip_reg, instructions} = parse_input(input)
    
    Stream.iterate({List.to_tuple([0,0,0,0,0,0]), 0}, fn {regs, _} ->
      ip = elem(regs, ip_reg)
      
      if ip >= tuple_size(instructions) do
        {:halt, regs}
      else
        inst = elem(instructions, ip)
        new_regs = execute_instruction(inst, regs)
        new_ip = elem(new_regs, ip_reg) + 1
        {put_elem(new_regs, ip_reg, new_ip), new_ip}
      end
    end)
    |> Enum.find(fn 
      {:halt, _} -> false
      {regs, ip} when ip == 28 -> true 
      _ -> false
    end)
    |> elem(0)
    |> elem(5)
  end

  def parse_input(input) do
    [ip_line | inst_lines] = String.split(input, "\n", trim: true)
    
    ip_reg = Regex.run(~r/#ip (\d)/, ip_line) 
    |> Enum.at(1) 
    |> String.to_integer()

    instructions = inst_lines
    |> Enum.map(fn line -> 
      [op | args] = String.split(line)
      args = Enum.map(args, &String.to_integer/1)
      {op, List.to_tuple(args)}
    end)
    |> List.to_tuple()

    {ip_reg, instructions}
  end

  def execute_instruction({op, {a, b, c}}, regs) do
    case op do
      "addr" -> put_elem(regs, c, elem(regs, a) + elem(regs, b))
      "addi" -> put_elem(regs, c, elem(regs, a) + b)
      "mulr" -> put_elem(regs, c, elem(regs, a) * elem(regs, b))
      "muli" -> put_elem(regs, c, elem(regs, a) * b)
      "banr" -> put_elem(regs, c, Bitwise.band(elem(regs, a), elem(regs, b)))
      "bani" -> put_elem(regs, c, Bitwise.band(elem(regs, a), b))
      "borr" -> put_elem(regs, c, Bitwise.bor(elem(regs, a), elem(regs, b)))
      "bori" -> put_elem(regs, c, Bitwise.bor(elem(regs, a), b))
      "setr" -> put_elem(regs, c, elem(regs, a))
      "seti" -> put_elem(regs, c, a)
      "gtir" -> put_elem(regs, c, if(a > elem(regs, b), do: 1, else: 0))
      "gtri" -> put_elem(regs, c, if(elem(regs, a) > b, do: 1, else: 0))
      "gtrr" -> put_elem(regs, c, if(elem(regs, a) > elem(regs, b), do: 1, else: 0))
      "eqir" -> put_elem(regs, c, if(a == elem(regs, b), do: 1, else: 0))
      "eqri" -> put_elem(regs, c, if(elem(regs, a) == b, do: 1, else: 0))
      "eqrr" -> put_elem(regs, c, if(elem(regs, a) == elem(regs, b), do: 1, else: 0))
    end
  end

  def main do
    {:ok, input} = File.read("input.txt")
    result = solve(input)
    IO.puts(result)
  end
end

Solution.main()
