
defmodule SantaHelper do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.filter(&nice?/1)
    |> length()
  end

  defp nice?(string) do
    has_three_vowels?(string) && has_double_letter?(string) && !has_forbidden_substring?(string)
  end

  defp has_three_vowels?(string) do
    string
    |> String.graphemes()
    |> Enum.filter(&(&1 in ["a", "e", "i", "o", "u"]))
    |> length()
    |> Kernel.>=(3)
  end

  defp has_double_letter?(string) do
    String.graphemes(string)
    |> Enum.chunk_by(& &1)
    |> Enum.any?(fn chunk -> length(chunk) >= 2 end)
  end

  defp has_forbidden_substring?(string) do
    Enum.any?(["ab", "cd", "pq", "xy"], &String.contains?(string, &1))
  end
end
