
defmodule Main do
  def main do
    sum =
      File.stream!("input.txt")
      |> Stream.map(&String.trim/1)
      |> Stream.filter(&(&1 != ""))
      |> Stream.map(&evaluate/1)
      |> Enum.sum()

    IO.puts(sum)
  end

  defp evaluate(expr) do
    tokens = tokenize(expr)
    {_, [result]} = eval_tokens(tokens, {[], []})
    result
  end

  defp tokenize(expr) do
    expr
    |> String.replace("(", "( ")
    |> String.replace(")", " )")
    |> String.split(~r/\s+/, trim: true)
  end

  defp eval_tokens([], {ops, vals}) do
    {ops, vals} = apply_remaining_ops(ops, vals)
    {ops, vals}
  end

  defp eval_tokens([token | rest], {ops, vals}) do
    case token do
      "(" ->
        eval_tokens(rest, {[token | ops], vals})

      "+" ->
        {ops, vals} = apply_until_parenthesis(ops, vals)
        eval_tokens(rest, {[token | ops], vals})

      "*" ->
        {ops, vals} = apply_until_parenthesis(ops, vals)
        eval_tokens(rest, {[token | ops], vals})

      ")" ->
        {ops, vals} = apply_until_parenthesis(ops, vals)
        eval_tokens(rest, {tl(ops), vals})

      _ ->
        value = String.to_integer(token)
        eval_tokens(rest, {ops, [value | vals]})
    end
  end

  defp apply_until_parenthesis(ops, vals) do
    case ops do
      [] -> {ops, vals}
      ["(" | _] -> {ops, vals}
      [op | rest_ops] ->
        [a, b | rest_vals] = vals
        result = apply_op(op, b, a)
        apply_until_parenthesis(rest_ops, [result | rest_vals])
    end
  end

  defp apply_remaining_ops(ops, vals) do
    case ops do
      [] -> {ops, vals}
      [op | rest_ops] ->
        [a, b | rest_vals] = vals
        result = apply_op(op, b, a)
        apply_remaining_ops(rest_ops, [result | rest_vals])
    end
  end

  defp apply_op("+", a, b), do: a + b
  defp apply_op("*", a, b), do: a * b
end

Main.main()
