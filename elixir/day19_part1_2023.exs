
defmodule AdventOfCode do
  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        [workflows_str, parts_str] = String.split(content, "\n\n")
        workflows = parse_workflows(workflows_str)
        parts = parse_parts(parts_str)

        total_sum =
          Enum.reduce(parts, 0, fn part, acc ->
            case process_part(workflows, part) do
              :accepted ->
                part["x"] + part["m"] + part["a"] + part["s"] + acc
              :rejected ->
                acc
            end
          end)

        IO.puts(total_sum)

      {:error, reason} ->
        IO.puts("Error reading file: #{reason}")
    end
  end

  defp parse_workflows(workflows_str) do
    workflows_str
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_workflow/1)
    |> Map.new()
  end

  defp parse_workflow(line) do
    [name, rules_str] = String.split(line, "{")
    name_atom = String.to_atom(name)
    rules =
      rules_str
      |> String.replace("}", "")
      |> String.split(",", trim: true)
      |> Enum.map(&parse_rule/1)

    {name_atom, rules}
  end

  defp parse_rule(rule_str) do
    case String.split(rule_str, ":", parts: 2) do
      [condition_str, destination_str] ->
        condition_fun = parse_condition(condition_str)
        destination_atom = String.to_atom(destination_str)
        {condition_fun, destination_atom}
      [destination_str] ->
        String.to_atom(destination_str)
    end
  end

  defp parse_condition(condition_str) do
    cond do
      String.contains?(condition_str, "<") ->
        [key, value_str] = String.split(condition_str, "<")
        value = String.to_integer(value_str)
        fn part -> part[key] < value end
      String.contains?(condition_str, ">") ->
        [key, value_str] = String.split(condition_str, ">")
        value = String.to_integer(value_str)
        fn part -> part[key] > value end
    end
  end

  defp parse_parts(parts_str) do
    parts_str
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_part/1)
  end

  defp parse_part(line) do
    line
    |> String.replace(["{", "}"], "")
    |> String.split(",", trim: true)
    |> Enum.map(&parse_rating/1)
    |> Map.new()
  end

  defp parse_rating(rating_str) do
    [key, value_str] = String.split(rating_str, "=")
    {key, String.to_integer(value_str)}
  end

  defp process_part(workflows, part) do
    apply_workflow(workflows, part, :in)
  end

  defp apply_workflow(_workflows, _part, :A), do: :accepted
  defp apply_workflow(_workflows, _part, :R), do: :rejected

  defp apply_workflow(workflows, part, workflow_name) do
    rules = Map.fetch!(workflows, workflow_name)
    apply_rules(workflows, part, rules)
  end

  defp apply_rules(workflows, part, [{condition_fun, destination} | rest]) do
    if condition_fun.(part) do
      apply_workflow(workflows, part, destination)
    else
      apply_rules(workflows, part, rest)
    end
  end

  defp apply_rules(workflows, part, [destination]), do: apply_workflow(workflows, part, destination)

end

# Run the main function when the script is executed
AdventOfCode.main()
