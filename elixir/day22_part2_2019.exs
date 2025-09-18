
defmodule Solver do
  @size 119315717514047
  @iterations 101741582076661
  @target_pos 2020

  def mod_add(a, b, m), do: Integer.mod(a + b, m)
  def mod_sub(a, b, m), do: Integer.mod(a - b, m)
  def mod_mul(a, b, m), do: Integer.mod(a * b, m)

  def mod_pow(base, exp, mod) do
    loop(base |> rem(mod), exp, mod, 1)
  end

  defp loop(_base, 0, _mod, acc), do: acc
  defp loop(base, exp, mod, acc) do
    if rem(exp, 2) == 1 do
      acc1 = mod_mul(acc, base, mod)
      loop(mod_mul(base, base, mod), div(exp, 2), mod, acc1)
    else
      loop(mod_mul(base, base, mod), div(exp, 2), mod, acc)
    end
  end

  def mod_inverse(n, mod) do
    mod_pow(n, mod - 2, mod)
  end

  def main do
    input = File.read!("input.txt")
    lines = String.split(input, "\n", trim: true)

    {offset, increment} =
      Enum.reduce(lines, {0, 1}, fn line, {offset_acc, inc_acc} ->
        l = String.trim(line)

        cond do
          l == "deal into new stack" ->
            inc = mod_mul(inc_acc, @size - 1, @size)
            {mod_add(offset_acc, inc, @size), inc}

          String.starts_with?(l, "cut") ->
            [_, n_str] = String.split(l, " ")
            n = String.to_integer(n_str)
            n_mod = Integer.mod(n, @size)
            {mod_add(offset_acc, mod_mul(n_mod, inc_acc, @size), @size), inc_acc}

          String.starts_with?(l, "deal with increment") ->
            tokens = String.split(l)
            n_str = List.last(tokens)
            n = String.to_integer(n_str)
            inv = mod_inverse(n, @size)
            {offset_acc, mod_mul(inc_acc, inv, @size)}

          true ->
            {offset_acc, inc_acc}
        end
      end)

    final_increment = mod_pow(increment, @iterations, @size)

    final_offset =
      if increment == 1 do
        mod_mul(offset, @iterations, @size)
      else
        term1 = mod_sub(final_increment, 1, @size)
        denom = mod_sub(increment, 1, @size)
        inv_denom = mod_inverse(denom, @size)
        offset
        |> mod_mul(term1, @size)
        |> mod_mul(inv_denom, @size)
      end

    answer = mod_add(mod_mul(@target_pos, final_increment, @size), final_offset, @size)
    IO.puts(answer)
  end
end

Solver.main()
