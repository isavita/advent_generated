
defmodule Solution do
  def main do
    data = File.read!("input.txt")
    lines = String.split(data, "\n", trim: true)

    {a, b, c, program} =
      Enum.reduce(lines, {0, 0, 0, []}, fn line, {a, b, c, program} ->
        cond do
          String.starts_with?(line, "Register A:") ->
            {String.to_integer(String.trim(String.split(line, ":", parts: 2) |> Enum.at(1))), b, c, program}

          String.starts_with?(line, "Register B:") ->
            {a, String.to_integer(String.trim(String.split(line, ":", parts: 2) |> Enum.at(1))), c, program}

          String.starts_with?(line, "Register C:") ->
            {a, b, String.to_integer(String.trim(String.split(line, ":", parts: 2) |> Enum.at(1))), program}

          String.starts_with?(line, "Program:") ->
            p_str = String.trim(String.split(line, ":", parts: 2) |> Enum.at(1))
            nums = String.split(p_str, ",", trim: true) |> Enum.map(&String.to_integer/1)
            {a, b, c, nums}

          true ->
            {a, b, c, program}
        end
      end)

    process(program, a, b, c, 0, [])
  end

  defp get_combo_val(op, a, b, c) do
    case op do
      n when n <= 3 -> n
      4 -> a
      5 -> b
      6 -> c
    end
  end

  defp process(_program, _a, _b, _c, ip, output) when ip >= length(_program), do: Enum.join(output, ",") |> IO.puts()

  defp process(program, a, b, c, ip, output) do
    opcode = Enum.at(program, ip)
    operand = Enum.at(program, ip + 1, -1)
    if operand == -1 do
        Enum.join(output, ",") |> IO.puts()
    else
    case opcode do
      0 ->
        den = get_combo_val(operand, a, b, c)
        a = if den == 0, do: 0, else: div(a, round(:math.pow(2, den)))
        process(program, a, b, c, ip + 2, output)
      1 ->
        process(program, a, Bitwise.bxor(b, operand), c, ip + 2, output)
      2 ->
        process(program, a, rem(get_combo_val(operand, a, b, c), 8), c, ip + 2, output)
      3 ->
        if a != 0 do
          process(program, a, b, c, operand, output)
        else
          process(program, a, b, c, ip + 2, output)
        end
      4 ->
        process(program, a, Bitwise.bxor(b, c), c, ip + 2, output)
      5 ->
        process(program, a, b, c, ip + 2, output ++ [Integer.to_string(rem(get_combo_val(operand, a, b, c), 8))])
      6 ->
        den = get_combo_val(operand, a, b, c)
        b = div(a, round(:math.pow(2, den)))
        process(program, a, b, c, ip + 2, output)
      7 ->
        den = get_combo_val(operand, a, b, c)
        c = div(a, round(:math.pow(2, den)))
        process(program, a, b, c, ip + 2, output)
    end
    end
  end
end

Solution.main()
