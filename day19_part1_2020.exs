
defmodule MonsterMessages do
  def call do
    input = File.read!("input.txt")
    {rules, messages} = String.split(input, "\n\n")

    rules_map = parse_rules(rules)
    valid_messages = count_valid_messages(rules_map, messages)

    valid_messages
  end

  defp parse_rules(rules) do
    rules
    |> String.split("\n")
    |> Enum.reduce(%{}, fn rule, acc ->
      [rule_id, rule_body] = String.split(rule, ": ")
      acc |> Map.put(rule_id, rule_body)
    end)
  end

  defp count_valid_messages(rules_map, messages) do
    messages
    |> String.split("\n")
    |> Enum.count(fn message ->
      Regex.match?(~r/\A#{expand_rule(rules_map, "0")}\z/, message)
    end)
  end

  defp expand_rule(rules_map, rule_id) do
    rule = Map.get(rules_map, rule_id)

    if String.contains?(rule, "|") do
      [left, right] = String.split(rule, " | ")
      "(?:#{expand_subrule(rules_map, left)}|#{expand_subrule(rules_map, right)})"
    else
      expand_subrule(rules_map, rule)
    end
  end

  defp expand_subrule(rules_map, subrule) do
    Enum.reduce(String.split(subrule), "", fn part, acc ->
      if part in ["a", "b"] do
        acc <> part
      else
        acc <> "(?:" <> expand_rule(rules_map, part) <> ")"
      end
    end)
  end
end
