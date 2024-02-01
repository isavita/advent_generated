
defmodule Matchsticks do
  def call do
    input = File.read!("input.txt")
    lines = String.split(input, "\n", trim: true)

    part1 = Enum.reduce(lines, 0, fn line, acc ->
      code_chars = String.length(line)
      memory_chars = String.length(eval(line))
      acc + (code_chars - memory_chars)
    end)

    part2 = Enum.reduce(lines, 0, fn line, acc ->
      encoded = encode(line)
      encoded_chars = String.length(encoded)
      code_chars = String.length(line)
      acc + (encoded_chars - code_chars)
    end)

    {part1, part2}
  end

  defp eval(string) do
    string
    |> String.slice(1..-2)
    |> String.replace("\\\"", "\"")
    |> String.replace("\\\\", "\\")
    |> String.replace(~r/\\x[0-9A-Fa-f]{2}/, fn _ -> "X" end)
  end

  defp encode(string) do
    string
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> (&("\"" <> &1 <> "\"")).()
  end
end
