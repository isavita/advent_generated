
defmodule BagCounter do
  def main do
    rules = parse_rules("input.txt")
    total_bags = count_bags("shiny gold", rules) - 1
    IO.puts(total_bags)
  end

  defp parse_rules(file) do
    File.stream!(file)
    |> Enum.reduce(%{}, fn line, acc ->
      [container, contents] = String.split(line, " bags contain ")
      contents = String.trim_trailing(contents, ".")
      
      if contents == "no other bags" do
        acc
      else
        rules = Regex.scan(~r/(\d+) (\w+ \w+) bags?/, contents)
        rules = Enum.map(rules, fn [_, count, color] -> {color, String.to_integer(count)} end)
        Map.update(acc, container, rules, &(&1 ++ rules))
      end
    end)
  end

  defp count_bags(color, rules) do
    case Map.get(rules, color) do
      nil -> 1
      bag_rules -> 
        1 + Enum.reduce(bag_rules, 0, fn {color, count}, acc ->
          acc + count * count_bags(color, rules)
        end)
    end
  end
end

BagCounter.main()
