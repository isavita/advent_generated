defmodule Solver do
  import Bitwise

  def compute_operand(val, a, b, c) when val in [0, 1, 2, 3], do: val
  def compute_operand(4, a, _b, _c), do: a
  def compute_operand(5, _a, b, _c), do: b
  def compute_operand(6, _a, _b, c), do: c
  def compute_operand(val, _a, _b, _c) do
    raise "Invalid combo operand: #{val}"
  end

  def mod8(x) do
    r = rem(x, 8)
    if r < 0, do: r + 8, else: r
  end

  def simulate(%{program: prog, a: a, b: b, c: c}) do
    do_sim(prog, a, b, c, 0, []) |> Enum.reverse()
  end

  defp do_sim(prog, _a, _b, _c, i, outs) when i >= length(prog), do: outs

  defp do_sim(prog, a, b, c, i, outs) do
    cmd = Enum.at(prog, i)
    i1 = i + 1

    case cmd do
      0 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        new_a = a >>> compute_operand(op, a, b, c)
        do_sim(prog, new_a, b, c, i2, outs)

      1 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        new_b = b ^^^ op
        do_sim(prog, a, new_b, c, i2, outs)

      2 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        new_b = mod8(compute_operand(op, a, b, c))
        do_sim(prog, a, new_b, c, i2, outs)

      3 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        new_i = if a != 0, do: op - 1, else: i2 - 1
        new_i = if new_i < length(prog), do: new_i + 1, else: new_i
        do_sim(prog, a, b, c, new_i, outs)

      4 ->
        new_b = b ^^^ c
        do_sim(prog, a, new_b, c, i1 + 1, outs)

      5 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        val = mod8(compute_operand(op, a, b, c))
        do_sim(prog, a, b, c, i2, [val | outs])

      6 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        new_b = a >>> compute_operand(op, a, b, c)
        do_sim(prog, a, new_b, c, i2, outs)

      7 ->
        op = Enum.at(prog, i1)
        i2 = i1 + 1
        new_c = a >>> compute_operand(op, a, b, c)
        do_sim(prog, a, b, new_c, i2, outs)

      _ ->
        raise "Invalid opcode: #{cmd}"
    end
  end

  def check(program) do
    do_check(program, [{0, 0}], MapSet.new(), [])
  end

  defp do_check(_program, [], _seen, acc), do: acc

  defp do_check(program, [{depth, score} | rest], seen, acc) do
    key = {depth, score}
    if MapSet.member?(seen, key) do
      do_check(program, rest, seen, acc)
    else
      seen2 = MapSet.put(seen, key)
      if depth == length(program[:program]) do
        acc2 = [score | acc]
        do_check(program, rest, seen2, acc2)
      else
        new_entries =
          for i <- 0..7 do
            new_score = i + 8 * score
            test_prog = %{a: new_score, b: program[:b], c: program[:c], program: program[:program]}
            result = simulate(test_prog)
            last_index = length(program[:program]) - 1 - depth
            expected = Enum.at(program[:program], last_index)
            if result != [] and List.first(result) == expected, do: {depth + 1, new_score}, else: nil
          end
        new_entries = Enum.filter(new_entries, & &1)
        do_check(program, new_entries ++ rest, seen2, acc)
      end
    end
  end
end

defmodule Main do
  def main do
    content = File.read!("input.txt")

    {a, b, c, program} = parse(content)

    graph = %{a: a, b: b, c: c, program: program}
    valid_values = Solver.check(graph)

    IO.puts(Enum.min(valid_values))
  end

  defp parse(content) do
    lines =
      content
      |> String.split("\n", trim: true)

    Enum.reduce(lines, {0, 0, 0, []}, fn line, {aa, bb, cc, prog} ->
      cond do
        String.starts_with?(line, "Register A:") ->
          val =
            line
            |> String.split(":", parts: 2)
            |> List.last()
            |> String.trim()
            |> String.to_integer()

          {val, bb, cc, prog}

        String.starts_with?(line, "Register B:") ->
          val =
            line
            |> String.split(":", parts: 2)
            |> List.last()
            |> String.trim()
            |> String.to_integer()

          {aa, val, cc, prog}

        String.starts_with?(line, "Register C:") ->
          val =
            line
            |> String.split(":", parts: 2)
            |> List.last()
            |> String.trim()
            |> String.to_integer()

          {aa, bb, val, prog}

        String.starts_with?(line, "Program:") ->
          vals =
            line
            |> String.split(":", parts: 2)
            |> List.last()
            |> String.trim()
            |> String.split(",")
            |> Enum.map(&String.trim/1)
            |> Enum.map(&String.to_integer/1)

          {aa, bb, cc, vals}

        true ->
          {aa, bb, cc, prog}
      end
    end)
  end
end

Main.main()