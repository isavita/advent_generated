
defmodule PatternMatcher do
  def can_make(design, patterns) do
    n = String.length(design)
    dp = Enum.to_list(List.duplicate(false, n + 1)) |> List.replace_at(0, true)

    Enum.reduce(1..n, dp, fn i, acc ->
      if Enum.any?(patterns, fn p ->
           lp = String.length(p)
           i >= lp and Enum.at(acc, i - lp) and
             String.slice(design, i - lp, lp) == p
         end) do
        List.replace_at(acc, i, true)
      else
        acc
      end
    end)
    |> Enum.at(n)
  end

  def main(filename) do
    {patterns, designs} =
      File.read!(filename)
      |> String.split("\n", trim: true)
      |> case do
        [patterns_line | rest] ->
          patterns =
            String.split(patterns_line, ",", trim: true)
            |> Enum.map(&String.strip/1)

          {patterns, rest}

        [] ->
          {[], []}
      end

    designs
    |> Enum.count(fn design -> can_make(design, patterns) end)
    |> IO.puts()
  end
end

PatternMatcher.main("input.txt")
