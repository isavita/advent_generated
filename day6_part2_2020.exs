
defmodule Customs do
  def call do
    input = File.read!("input.txt")
    {part1, part2} = process_input(input)
    {part1, part2}
  end

  defp process_input(input) do
    groups = String.split(input, "\n\n", trim: true)
    {Enum.sum(Enum.map(groups, &count_anyone_yes(&1))), Enum.sum(Enum.map(groups, &count_everyone_yes(&1)))}
  end

  defp count_anyone_yes(group) do
    group
    |> String.replace("\n", "")
    |> String.graphemes()
    |> Enum.uniq()
    |> length()
  end

  defp count_everyone_yes(group) do
    people = String.split(group, "\n", trim: true)
    people_count = length(people)
    answers = Enum.map(people, &String.graphemes(&1))
    Enum.reduce(answers, MapSet.new(), &MapSet.union(&2, MapSet.new(&1)))
    |> MapSet.to_list()
    |> Enum.filter(fn question ->
      Enum.all?(answers, &Enum.member?(&1, question))
    end)
    |> length()
  end
end
