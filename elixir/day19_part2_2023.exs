
defmodule Day19 do
  def parse_input(input) do
    [workflows_str, parts_str] = String.split(input, "\n\n", trim: true)

    workflows =
      workflows_str
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_workflow/1)
      |> Enum.into(%{})

    parts =
      parts_str
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_part/1)

    {workflows, parts}
  end

  defp parse_workflow(line) do
    [name, rules_str] = String.split(line, "{", parts: 2)
    rules_str = String.replace(rules_str, "}", "")
    rules = String.split(rules_str, ",") |> Enum.map(&parse_rule/1)
    {String.to_atom(name), rules}
  end

  defp parse_rule(rule_str) do
    if String.contains?(rule_str, ":") do
      [condition, destination] = String.split(rule_str, ":")
      [category, operator, value] =
        Regex.run(~r/([a-z]+)([<>])([0-9]+)/, condition, capture: :all_but_first)

      {String.to_atom(category), operator, String.to_integer(value), String.to_atom(destination)}
    else
      {nil, nil, nil, String.to_atom(rule_str)}
    end
  end

  defp parse_part(line) do
    line
    |> String.replace(~r/[{}a-z=]/, "")
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> then(fn [x, m, a, s] -> %{x: x, m: m, a: a, s: s} end)
  end

  def process_part(part, workflows, workflow_name \\ :in) do
    case workflow_name do
      :A ->
        part

      :R ->
        nil

      _ ->
        rules = workflows[workflow_name]

        rules
        |> Enum.find_value(fn
          {nil, nil, nil, next_workflow} ->
            process_part(part, workflows, next_workflow)

          {category, operator, value, next_workflow} ->
            if apply_condition(part, category, operator, value) do
              process_part(part, workflows, next_workflow)
            else
              nil
            end
        end)
    end
  end

  defp apply_condition(part, category, operator, value) do
    case operator do
      ">" -> part[category] > value
      "<" -> part[category] < value
    end
  end

  def part1(input) do
    {workflows, parts} = parse_input(input)

    parts
    |> Enum.filter_map(&process_part(&1, workflows), fn part -> part end)
    |> Enum.reduce(0, fn part, acc ->
      acc + part.x + part.m + part.a + part.s
    end)
  end

  def count_combinations(workflows, ranges, workflow_name \\ :in) do
    case workflow_name do
      :A ->
        ranges
        |> Map.values()
        |> Enum.map(fn {min, max} -> max - min + 1 end)
        |> Enum.reduce(1, &*/2)

      :R ->
        0

      _ ->
        rules = workflows[workflow_name]
        process_rules(rules, ranges, workflows)
    end
  end

  defp process_rules([{nil, nil, nil, next_workflow} | rest], ranges, workflows) do
    count_combinations(workflows, ranges, next_workflow) +
      if length(rest) > 0 do
        0
      else
        0
      end
  end

  defp process_rules([{category, operator, value, next_workflow} | rest], ranges, workflows) do
    {min, max} = ranges[category]

    {true_range, false_range} =
      case operator do
        "<" -> {{min, min(value - 1, max)}, {max(value, min), max}}
        ">" -> {{max(value + 1, min), max}, {min, min(value, max)}}
      end
    
    true_count = if elem(true_range, 1) >= elem(true_range, 0) do
      count_combinations(workflows, Map.put(ranges, category, true_range), next_workflow)
    else
      0
    end

    false_count = if elem(false_range, 1) >= elem(false_range, 0) and length(rest) > 0 do
      process_rules(rest, Map.put(ranges, category, false_range), workflows)
    else
      0
    end

      
    true_count + false_count

  end

  def part2(input) do
    {workflows, _parts} = parse_input(input)

    initial_ranges = %{
      x: {1, 4000},
      m: {1, 4000},
      a: {1, 4000},
      s: {1, 4000}
    }

    count_combinations(workflows, initial_ranges)
  end
end

input = File.read!("input.txt")

IO.puts("Part 1: #{Day19.part1(input)}")
IO.puts("Part 2: #{Day19.part2(input)}")
