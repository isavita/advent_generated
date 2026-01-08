
defmodule Main do
  import Bitwise

  # -------------------------------------------------
  # IO helpers
  # -------------------------------------------------
  def read_file(path) do
    File.read!(path)
    |> String.split("\n")
  end

  # -------------------------------------------------
  # Parsing – split the input into blocks separated by blank lines
  # -------------------------------------------------
  def parse_blocks(lines) do
    lines
    |> Enum.reduce([[]], fn
      "", [current | rest] -> [[] | [current | rest]]
      line, [current | rest] -> [[line | current] | rest]
    end)
    |> Enum.map(&Enum.reverse/1)
    |> Enum.reject(&(&1 == []))
  end

  # -------------------------------------------------
  # Convert a block of strings into bit‑encoded rows and columns
  # -------------------------------------------------
  def rows_and_cols(block) do
    rows =
      Enum.map(block, fn line ->
        line
        |> String.graphemes()
        |> Enum.reduce(0, fn ch, acc -> (acc <<< 1) + if ch == "#", do: 1, else: 0 end)
      end)

    width = String.length(List.first(block))

    cols =
      for x <- 0..(width - 1) do
        Enum.reduce(Enum.with_index(block), 0, fn {line, y}, acc ->
          ch = String.at(line, x)

          (acc <<< 1) + if ch == "#", do: 1, else: 0
        end)
      end

    {rows, cols}
  end

  # -------------------------------------------------
  # Find the mirror axis that needs exactly ONE smudge
  # -------------------------------------------------
  def get_mirror_axis_with_one_smudge(lines) do
    len = length(lines)

    1..(len - 1)
    |> Enum.find(0, fn i -> axis_ok?(lines, i) end)
  end

  defp axis_ok?(lines, i) do
    len = length(lines)
    max = min(i, len - i)

    {smudges, ok} =
      Enum.reduce_while(0..(max - 1), {0, true}, fn j, {smudges, _} ->
        a = Enum.at(lines, i - 1 - j)
        b = Enum.at(lines, i + j)

        if a == b do
          {:cont, {smudges, true}}
        else
          diff = bxor(a, b)

          cond do
            smudges == 0 and single_bit?(diff) ->
              {:cont, {1, true}}

            true ->
              {:halt, {smudges, false}}
          end
        end
      end)

    ok and smudges == 1
  end

  defp single_bit?(0), do: false
  defp single_bit?(n), do: (n &&& (n - 1)) == 0

  # -------------------------------------------------
  # Solve the whole puzzle
  # -------------------------------------------------
  def solve(lines) do
    parse_blocks(lines)
    |> Enum.reduce(0, fn block, acc ->
      {rows, cols} = rows_and_cols(block)

      acc +
        get_mirror_axis_with_one_smudge(cols) +
        100 * get_mirror_axis_with_one_smudge(rows)
    end)
  end

  # -------------------------------------------------
  # Entry point
  # -------------------------------------------------
  def main(_args) do
    result = "input.txt" |> read_file() |> solve()
    IO.puts(result)
  end
end

# When the file is executed directly (e.g. `elixir solution.exs`)
Main.main([])
