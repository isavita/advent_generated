defmodule BadgeIdentification do
  def run do
    input = File.read!("input.txt")
    rucksacks = String.split(input, "\n", trim: true)
    groups = Enum.chunk_every(rucksacks, 3)
    common_items_priorities = Enum.map(groups, &process_group/1)
    sum_of_priorities = Enum.sum(common_items_priorities)
    IO.puts("Sum of priorities: #{sum_of_priorities}")
  end

  defp process_group(group) do
    common_items = Enum.reduce(group, MapSet.new(String.graphemes(hd(group))), fn rucksack, acc ->
      MapSet.intersection(acc, MapSet.new(String.graphemes(rucksack)))
    end)

    item_priority(Enum.at(MapSet.to_list(common_items), 0))
  end

  defp item_priority(item) do
    char_code = item |> String.to_charlist() |> hd()

    cond do
      char_code >= ?a and char_code <= ?z -> char_code - ?a + 1
      char_code >= ?A and char_code <= ?Z -> char_code - ?A + 27
    end
  end
end

BadgeIdentification.run()
