
defmodule Day18 do
  def solve(filename, precedence) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&evaluate(&1, precedence))
    |> Enum.sum()
  end

  def evaluate(expression, precedence) do
    expression
    |> tokenize()
    |> to_postfix(precedence)
    |> eval_postfix()
  end

  def tokenize(expression) do
    expression
    |> String.replace("(", " ( ")
    |> String.replace(")", " ) ")
    |> String.split(" ", trim: true)
  end
  
  def to_postfix(tokens, precedence) do
    do_to_postfix(tokens, [], [], precedence)
  end

  defp do_to_postfix([], stack, output, _precedence), do: output ++ stack

  defp do_to_postfix(["(" | rest], stack, output, precedence) do
    do_to_postfix(rest, ["(" | stack], output, precedence)
  end

  defp do_to_postfix([")" | rest], stack, output, precedence) do
    {until_paren, rest_stack} = Enum.split_while(stack, &(&1 != "("))
    do_to_postfix(rest, tl(rest_stack), output ++ until_paren, precedence)
  end

  defp do_to_postfix([op | rest], stack, output, precedence) when op in ["+", "*"] do
    {higher_prec, rest_stack} =
      Enum.split_while(stack, &(is_operator(&1) and precedence[op] <= precedence[&1]))

    do_to_postfix(rest, [op | rest_stack], output ++ higher_prec, precedence)
  end

  defp do_to_postfix([num | rest], stack, output, precedence) do
    do_to_postfix(rest, stack, output ++ [num], precedence)
  end

  defp is_operator(token) do
    token in ["+", "*"]
  end

  def eval_postfix(postfix_tokens) do
    postfix_tokens
    |> Enum.reduce([], &reduce_postfix/2)
    |> hd()
    |> String.to_integer()
  end

  defp reduce_postfix(token, stack) when token in ["+", "*"] do
    [b, a | rest] = stack
    result =
      case token do
        "+" -> String.to_integer(a) + String.to_integer(b)
        "*" -> String.to_integer(a) * String.to_integer(b)
      end

    [Integer.to_string(result) | rest]
  end

  defp reduce_postfix(token, stack) do
    [token | stack]
  end

  def part1(filename) do
    precedence = %{"+" => 1, "*" => 1}
    solve(filename, precedence)
  end

  def part2(filename) do
    precedence = %{"+" => 2, "*" => 1}
    solve(filename, precedence)
  end
end

# Run the solver
result1 = Day18.part1("input.txt")
IO.puts("Part 1 Result: #{result1}")

result2 = Day18.part2("input.txt")
IO.puts("Part 2 Result: #{result2}")

