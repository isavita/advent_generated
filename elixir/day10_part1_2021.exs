defmodule SyntaxScoring do
  def call do
    input = read_input()
    error_score = calculate_error_score(input)
    IO.puts(error_score)
  end

  defp read_input do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
  end

  defp calculate_error_score(lines) do
    lines
    |> Enum.map(&find_illegal_character/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&get_error_score/1)
    |> Enum.sum()
  end

  defp find_illegal_character(line) do
    chars = String.graphemes(line)
    find_illegal_character(chars, [])
  end

  defp find_illegal_character([], _stack), do: nil
  defp find_illegal_character([char | rest], stack) do
    case char do
      "(" -> find_illegal_character(rest, [")" | stack])
      "[" -> find_illegal_character(rest, ["]" | stack])
      "{" -> find_illegal_character(rest, ["}" | stack])
      "<" -> find_illegal_character(rest, [">" | stack])
      closing ->
        case stack do
          [^closing | remaining] -> find_illegal_character(rest, remaining)
          _ -> char
        end
    end
  end

  defp get_error_score(")"), do: 3
  defp get_error_score("]"), do: 57
  defp get_error_score("}"), do: 1197
  defp get_error_score(">"), do: 25137
end

SyntaxScoring.call()
