defmodule RucksackReorganization do
  def run do
    input = File.read!("input.txt")
    rucksacks = String.split(input, "\n", trim: true)
    common_items_priorities = Enum.map(rucksacks, &process_rucksack/1)
    sum_of_priorities = Enum.sum(common_items_priorities)
    IO.puts("Sum of priorities: #{sum_of_priorities}")
  end

  defp process_rucksack(rucksack) do
    mid = div(String.length(rucksack), 2)
    {compartment1, compartment2} = String.split_at(rucksack, mid)

    common_item = find_common_item(String.graphemes(compartment1), String.graphemes(compartment2))
    item_priority(common_item)
  end

  defp find_common_item(compartment1, compartment2) do
    Enum.find(compartment1, fn item -> item in compartment2 end)
  end

  defp item_priority(item) do
    char_code = item |> String.to_charlist() |> hd()

    cond do
      char_code >= ?a and char_code <= ?z -> char_code - ?a + 1
      char_code >= ?A and char_code <= ?Z -> char_code - ?A + 27
    end
  end
end

RucksackReorganization.run()
