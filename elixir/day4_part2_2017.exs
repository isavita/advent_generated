
defmodule PassphraseValidator do
  def call do
    input = File.read!("input.txt")
    {part1, part2} = Enum.map(input |> String.split("\n", trim: true), &process_passphrase/1) |> Enum.unzip()
    {Enum.count(part1, &(&1 == true)), Enum.count(part2, &(&1 == true))}
  end

  defp process_passphrase(passphrase) do
    words = String.split(passphrase)
    {no_duplicate_words?(words), no_anagram_words?(words)}
  end

  defp no_duplicate_words?(words) do
    Enum.uniq(words) == words
  end

  defp no_anagram_words?(words) do
    sorted_words = Enum.map(words, &String.graphemes(&1) |> Enum.sort() |> Enum.join())
    Enum.uniq(sorted_words) == sorted_words
  end
end
