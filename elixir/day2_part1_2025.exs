
defmodule Main do
  @pow10 Enum.reduce(0..20, %{}, fn i, acc ->
           Map.put(acc, i, if(i==0, do: 1, else: acc[i-1]*10))
         end)

  defp str_to_u128(s), do: String.to_integer(s)

  defp u128_to_str(n), do: Integer.to_string(n)

  defp process_range(start, stop, ids) when start > stop, do: process_range(stop, start, ids)
  defp process_range(start, stop, ids) do
    Enum.reduce(1..10, ids, fn k, acc ->
      multiplier = @pow10[k] + 1
      min_seed = @pow10[k-1]
      max_seed = @pow10[k] - 1
      s_min = max(min_seed, div(start + multiplier - 1, multiplier))
      s_max = min(max_seed, div(stop, multiplier))
      if s_min > s_max do
        acc
      else
        Enum.reduce(s_min..s_max, acc, fn seed, inner_acc ->
          [seed * multiplier | inner_acc]
        end)
      end
    end)
  end

  def main do
    ids =
      File.read!("input.txt")
      |> String.replace(~r/[\s\n\r]+/, ",")
      |> String.split(",")
      |> Enum.filter(&(&1 != ""))
      |> Enum.flat_map(fn token ->
        [a, b] = String.split(token, "-")
        process_range(str_to_u128(a), str_to_u128(b), [])
      end)
      |> Enum.sort()
      |> Enum.dedup()
      |> Enum.sum()
    IO.puts(u128_to_str(ids))
  end
end

Main.main()
