
defmodule BagRules do
  def parse_rule(line) do
    [container, contents] = String.split(line, " bags contain ")
    contents =
      if String.contains?(contents, "no other bags") do
        []
      else
        String.split(contents, ", ")
        |> Enum.map(fn content ->
          Regex.run(~r/(\d+) (\w+ \w+) bag[s]?/, content)
          |> List.delete_at(0)
          |> List.to_tuple()
        end)
      end

    {container, contents}
  end

  def build_graph(rules) do
    rules
    |> Enum.map(&parse_rule/1)
    |> Enum.reduce(%{}, fn {container, contents}, acc ->
      Map.put(acc, container, contents)
    end)
  end

  def can_contain?(graph, target_bag, current_bag) do
    case Map.get(graph, current_bag) do
      nil -> false
      contents ->
        Enum.any?(contents, fn {_, bag} ->
          bag == target_bag || can_contain?(graph, target_bag, bag)
        end)
    end
  end

  def count_containing_bags(graph, target_bag) do
    graph
    |> Map.keys()
    |> Enum.count(fn bag -> can_contain?(graph, target_bag, bag) end)
  end
end


input = File.read!("input.txt")
|> String.split("\n")
|> Enum.filter(&(&1 != ""))

graph = BagRules.build_graph(input)
result = BagRules.count_containing_bags(graph, "shiny gold")
IO.puts(result)

