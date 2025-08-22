
defmodule Main do
  def main do
    input = File.read!("input.txt")
    %{ip_reg: ip_reg, instrs: instrs} = parse(input)

    regs = {0, 0, 0, 0, 0, 0}
    loop(regs, ip_reg, instrs, MapSet.new(), 0)
    |> IO.puts()
  end

  defp parse(input) do
    [first | rest] = String.split(input, "\n", trim: true)
    [_ | [ip_str]] = String.split(first, " ")
    ip_reg = String.to_integer(ip_str)

    instrs =
      Enum.map(rest, fn line ->
        [op, a, b, c] = String.split(line, " ", trim: true)
        {op, String.to_integer(a), String.to_integer(b), String.to_integer(c)}
      end)

    %{ip_reg: ip_reg, instrs: instrs}
  end

  defp loop(regs, ip_reg, instrs, seen, last) do
    ip = elem(regs, ip_reg)

    if ip < 0 or ip >= length(instrs) do
      last
    else
      {op, a, b, c} = Enum.at(instrs, ip)
      regs = exec(regs, op, a, b, c)
      regs = put_elem(regs, ip_reg, elem(regs, ip_reg) + 1)

      if ip == 28 do
        r5 = elem(regs, 5)

        if MapSet.member?(seen, r5) do
          last
        else
          loop(regs, ip_reg, instrs, MapSet.put(seen, r5), r5)
        end
      else
        loop(regs, ip_reg, instrs, seen, last)
      end
    end
  end

  defp exec(regs, "addr", a, b, c), do: put_elem(regs, c, elem(regs, a) + elem(regs, b))
  defp exec(regs, "addi", a, b, c), do: put_elem(regs, c, elem(regs, a) + b)
  defp exec(regs, "mulr", a, b, c), do: put_elem(regs, c, elem(regs, a) * elem(regs, b))
  defp exec(regs, "muli", a, b, c), do: put_elem(regs, c, elem(regs, a) * b)
  defp exec(regs, "banr", a, b, c), do: put_elem(regs, c, Bitwise.band(elem(regs, a), elem(regs, b)))
  defp exec(regs, "bani", a, b, c), do: put_elem(regs, c, Bitwise.band(elem(regs, a), b))
  defp exec(regs, "borr", a, b, c), do: put_elem(regs, c, Bitwise.bor(elem(regs, a), elem(regs, b)))
  defp exec(regs, "bori", a, b, c), do: put_elem(regs, c, Bitwise.bor(elem(regs, a), b))
  defp exec(regs, "setr", a, _b, c), do: put_elem(regs, c, elem(regs, a))
  defp exec(regs, "seti", a, _b, c), do: put_elem(regs, c, a)
  defp exec(regs, "gtir", a, b, c), do: put_elem(regs, c, if(a > elem(regs, b), do: 1, else: 0))
  defp exec(regs, "gtri", a, b, c), do: put_elem(regs, c, if(elem(regs, a) > b, do: 1, else: 0))
  defp exec(regs, "gtrr", a, b, c), do: put_elem(regs, c, if(elem(regs, a) > elem(regs, b), do: 1, else: 0))
  defp exec(regs, "eqir", a, b, c), do: put_elem(regs, c, if(a == elem(regs, b), do: 1, else: 0))
  defp exec(regs, "eqri", a, b, c), do: put_elem(regs, c, if(elem(regs, a) == b, do: 1, else: 0))
  defp exec(regs, "eqrr", a, b, c), do: put_elem(regs, c, if(elem(regs, a) == elem(regs, b), do: 1, else: 0))
end

Main.main()
