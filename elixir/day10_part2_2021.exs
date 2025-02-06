
defmodule Day10 do
  def solve() do
    input = File.read!("input.txt")
    lines = String.split(input, "\n", trim: true)

    {part1_score, incomplete_lines} = corrupted_score_and_incomplete_lines(lines)
    IO.puts("Part 1: #{part1_score}")

    part2_score = autocomplete_score(incomplete_lines)
    IO.puts("Part 2: #{part2_score}")
  end

  defp corrupted_score_and_incomplete_lines(lines) do
    Enum.reduce(lines, {0, []}, fn line, {acc_score, acc_incomplete} ->
      case process_line(line) do
        {:corrupted, score} -> {acc_score + score, acc_incomplete}
        {:incomplete, stack} -> {acc_score, [stack | acc_incomplete]}
        :ok -> {acc_score, acc_incomplete} # Should not ideally happen, added for completeness
      end
    end)
  end

  defp process_line(line) do
    opening = %{
      "(" => ")",
      "[" => "]",
      "{" => "}",
      "<" => ">"
    }

    closing = %{
      ")" => 3,
      "]" => 57,
      "}" => 1197,
      ">" => 25137
    }

    line
    |> String.graphemes()
    |> Enum.reduce_while({:ok, []}, fn char, {:ok, stack} ->
      cond do
        Map.has_key?(opening, char) ->
          {:cont, {:ok, [opening[char] | stack]}}

        Map.has_key?(closing, char) ->
          case stack do
            [expected | rest] when expected == char ->
              {:cont, {:ok, rest}}

            _ ->
              {:halt, {:corrupted, closing[char]}}
          end

        true ->
          {:cont, {:ok, stack}} # Should not ideally happen, added for completeness.
      end
    end)
    |> case do
      {:corrupted, score} -> {:corrupted, score}
      {:ok, stack} -> if stack == [], do: :ok, else: {:incomplete, stack}
    end
  end

  defp autocomplete_score(incomplete_lines) do
      scores =
        Enum.map(incomplete_lines, fn stack ->
          Enum.reduce(stack, 0, fn char, acc ->
            score =
              case char do
                ")" -> 1
                "]" -> 2
                "}" -> 3
                ">" -> 4
              end

            acc * 5 + score
          end)
        end)
        |> Enum.sort()

      Enum.at(scores, div(length(scores), 2))
    end
end

Day10.solve()
