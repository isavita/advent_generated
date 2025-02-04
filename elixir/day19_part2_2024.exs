
defmodule Solution do
  def solve(filename \\ "input.txt") do
    [available_line, _ | designs] = File.read!(filename) |> String.split("\n")

    available_patterns =
      available_line
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.trim/1)

    designs
    |> Enum.reduce(0, fn design, acc ->
      acc + count_ways(String.trim(design), available_patterns)
    end)
    |> IO.puts()
  end

  defp count_ways(design, patterns) do
    n = String.length(design)
    dp = List.duplicate(0, n + 1) |> List.replace_at(0, 1)

    1..n
    |> Enum.reduce(dp, fn i, acc_dp ->
      patterns
      |> Enum.reduce(acc_dp, fn p, inner_acc_dp ->
        lp = String.length(p)

        if i >= lp and String.slice(design, i - lp, lp) == p do
          List.update_at(inner_acc_dp, i, &(&1 + Enum.at(inner_acc_dp, i - lp)))
        else
          inner_acc_dp
        end
      end)
    end)
    |> Enum.at(n)
  end
end

Solution.solve()
