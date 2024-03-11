defmodule ExtendedPolymerization do
  def call do
    {template, rules} = File.read!("input.txt") |> parse_input()

    final_counts =
      Enum.reduce(1..40, Map.new(enumerate_pairs(template)), fn _, acc ->
        apply_rules(acc, rules)
      end)

    element_counts = count_elements(final_counts, String.last(template))
    max_count = element_counts |> Enum.max_by(fn {_, v} -> v end) |> elem(1)
    min_count = element_counts |> Enum.min_by(fn {_, v} -> v end) |> elem(1)
    IO.puts(max_count - min_count)
  end

  defp parse_input(input) do
    [template, rule_section] = String.split(input, "\n\n", parts: 2, trim: true)

    rules =
      rule_section
      |> String.split("\n", trim: true)
      # Reject empty lines
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(fn line ->
        case String.split(line, " -> ") do
          [pair, insertion] -> {pair, insertion}
          _ -> nil
        end
      end)
      # Reject lines that do not match the expected format
      |> Enum.reject(&is_nil(&1))
      |> Map.new()

    {template, rules}
  end

  defp enumerate_pairs(template) do
    Enum.zip(String.codepoints(template), String.codepoints(template) |> Enum.drop(1))
    |> Enum.map(&(Tuple.to_list(&1) |> Enum.join()))
    |> Enum.frequencies()
  end

  defp apply_rules(pairs, rules) do
    Enum.reduce(pairs, %{}, fn {pair, count}, acc ->
      insertion = Map.get(rules, pair)
      left = Enum.at(String.codepoints(pair), 0) <> insertion
      right = insertion <> Enum.at(String.codepoints(pair), 1)

      acc
      |> Map.update(left, count, &(&1 + count))
      |> Map.update(right, count, &(&1 + count))
    end)
  end

  defp count_elements(pairs, last_element) do
    pairs
    |> Enum.reduce(%{last_element => 1}, fn {pair, count}, acc ->
      elem = Enum.at(String.codepoints(pair), 0)
      Map.update(acc, elem, count, &(&1 + count))
    end)
  end
end

ExtendedPolymerization.call()
