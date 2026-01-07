
defmodule Main do
  def main do
    ranges =
      File.stream!("input.txt")
      |> Stream.map(&String.trim_trailing/1)
      |> Stream.chunk_by(&(&1 == ""))
      |> Enum.take(2)
      |> hd()
      |> Enum.map(fn line ->
        [a, b] = String.split(line, "-")
        {String.to_integer(a), String.to_integer(b)}
      end)
      |> Enum.sort()
      |> merge([])

    ans =
      File.stream!("input.txt")
      |> Stream.drop(length(ranges) + 2)
      |> Stream.map(&String.trim_trailing/1)
      |> Stream.filter(&(&1 != ""))
      |> Enum.count(fn id ->
        id = String.to_integer(id)
        Enum.any?(ranges, fn {lo, hi} -> lo <= id and id <= hi end)
      end)

    IO.puts("Number of fresh ingredients: #{ans}")
  end

  defp merge([], acc), do: Enum.reverse(acc)
  defp merge([{a1, b1} | rest], []), do: merge(rest, [{a1, b1}])
  defp merge([{a1, b1} | rest], [{a0, b0} | tail]) when a1 <= b0 + 1,
    do: merge(rest, [{min(a0, a1), max(b0, b1)} | tail])
  defp merge([{a1, b1} | rest], acc), do: merge(rest, [{a1, b1} | acc])
end

Main.main()
