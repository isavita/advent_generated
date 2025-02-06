
defmodule Day19 do
  def parse_input(input) do
    [rules_str, messages_str] = String.split(input, "\n\n")

    rules =
      rules_str
      |> String.split("\n")
      |> Enum.map(fn rule ->
        [id, rest] = String.split(rule, ": ")
        {String.to_integer(id), parse_rule(rest)}
      end)
      |> Enum.into(%{})

    messages = String.split(messages_str, "\n")

    {rules, messages}
  end

  defp parse_rule(rule_str) do
    if String.starts_with?(rule_str, "\"") do
      String.at(rule_str, 1)
    else
      rule_str
      |> String.split(" | ")
      |> Enum.map(fn part ->
        part
        |> String.split(" ")
        |> Enum.map(&String.to_integer/1)
      end)
    end
  end

  def match?(rules, message, rule_id) do
    match?(rules, message, rule_id, 0) != []
  end

  defp match?(rules, message, rule_id, start_index) do
    case rules[rule_id] do
      char when is_binary(char) ->
        if String.at(message, start_index) == char do
          [start_index + 1]
        else
          []
        end

      rule_options ->
        Enum.flat_map(rule_options, fn option ->
          match_option(rules, message, option, start_index)
        end)
    end
  end

  defp match_option(rules, message, [], index) do
    [index]
  end

  defp match_option(rules, message, [rule_id | rest], index) do
    match?(rules, message, rule_id, index)
    |> Enum.flat_map(fn next_index ->
      match_option(rules, message, rest, next_index)
    end)
  end

  def solve(input, part) do
    {rules, messages} = parse_input(input)

    rules =
      if part == 2 do
        rules
        |> Map.put(8, [[42], [42, 8]])
        |> Map.put(11, [[42, 31], [42, 11, 31]])
      else
        rules
      end

    messages
    |> Enum.filter(fn message ->
      Enum.member?(match?(rules, message, 0, 0), String.length(message))
    end)
    |> length()
  end
end

input = File.read!("input.txt")

part1_result = Day19.solve(input, 1)
IO.puts("Part 1: #{part1_result}")

part2_result = Day19.solve(input, 2)
IO.puts("Part 2: #{part2_result}")
