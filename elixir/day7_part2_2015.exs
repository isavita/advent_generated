
defmodule Circuit do
  def main do
    rules = File.read!("input.txt") |> parse_rules()
    {a_signal_part1, _memo_part1} = solve(rules, "a", %{})

    rules_part2 = Map.put(rules, "b", Integer.to_string(a_signal_part1))
    {a_signal_part2, _memo_part2} = solve(rules_part2, "a", %{})

    IO.puts(a_signal_part2)
  end

  defp parse_rules(input_string) do
    input_string
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, acc ->
      [rule, wire] = String.split(line, " -> ")
      Map.put(acc, String.trim(wire), String.trim(rule))
    end)
  end

  defp solve(_rules, target, memo) when is_binary(target) do
    case Integer.parse(target) do
      {number, ""} -> {number, memo}
      :error -> solve_wire(_rules, target, memo)
    end
  end

  defp solve_wire(rules, wire, memo) do
    case Map.fetch(memo, wire) do
      {:ok, value} -> {value, memo}
      :error ->
        rule = Map.fetch!(rules, wire)
        {value, new_memo} =
          rule
          |> String.split(" ")
          |> case do
            [arg1] ->
              solve(rules, arg1, memo)
            ["NOT", arg1] ->
              {v1, m1} = solve(rules, arg1, memo)
              value = Bitwise.bxor(v1, 65535)
              {value, m1}
            [arg1, "AND", arg2] ->
              {v1, m1} = solve(rules, arg1, memo)
              {v2, m2} = solve(rules, arg2, m1)
              value = Bitwise.band(v1, v2)
              {value, m2}
            [arg1, "OR", arg2] ->
              {v1, m1} = solve(rules, arg1, memo)
              {v2, m2} = solve(rules, arg2, m1)
              value = Bitwise.bor(v1, v2)
              {value, m2}
            [arg1, "LSHIFT", arg2] ->
              {v1, m1} = solve(rules, arg1, memo)
              {v2, m2} = solve(rules, arg2, m1)
              value = Bitwise.bsl(v1, v2)
              {value, m2}
            [arg1, "RSHIFT", arg2] ->
              {v1, m1} = solve(rules, arg1, memo)
              {v2, m2} = solve(rules, arg2, m1)
              value = Bitwise.bsr(v1, v2)
              {value, m2}
          end
        {value, Map.put(new_memo, wire, value)}
    end
  end
end

Circuit.main()
