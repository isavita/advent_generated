
defmodule Day5 do
  def call do
    File.read!("input.txt")
    |> String.split("\n")
    |> Enum.filter(&is_nice_string/1)
    |> Enum.count()
  end

  defp is_vowel?(char) do
    ["a", "e", "i", "o", "u"] |> Enum.member?(String.downcase(char))
  end

  defp has_three_vowels?(string) do
    string
    |> String.graphemes()
    |> Enum.filter(&is_vowel?/1)
    |> Enum.count() >= 3
  end

  defp has_double_letter?(string) do
    string
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.any?(fn {char, idx} ->
      idx > 0 and char == String.graphemes(string) |> Enum.at(idx - 1)
    end)
  end

  defp has_disallowed_substring?(string) do
    ["ab", "cd", "pq", "xy"]
    |> Enum.any?(fn substring -> string |> String.contains?(substring) end)
  end

  defp is_nice_string(string) do
    has_three_vowels?(string) and
    has_double_letter?(string) and
    not has_disallowed_substring?(string)
  end

  defp has_repeated_pair?(string) do
    string
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.flat_map(fn {char, idx} ->
      idx > 1
      |> Kernel.&&()
      |> Kernel.&&(char == String.graphemes(string) |> Enum.at(idx - 2))
      |> Kernel.&&([char <> String.graphemes(string) |> Enum.at(idx - 1)])
    end)
    |> Enum.uniq()
    |> Enum.count() < Enum.uniq(string)
  end

  defp has_repeated_letter_with_one_between?(string) do
    string
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.any?(fn {char, idx} ->
      idx > 1 and
      idx < String.length(string) - 1 and
      char == String.graphemes(string) |> Enum.at(idx - 1) and
      char == String.graphemes(string) |> Enum.at(idx + 1)
    end)
  end

  defp is_nice_string_part_two(string) do
    has_repeated_pair?(string) and
    has_repeated_letter_with_one_between?(string)
  end
end
