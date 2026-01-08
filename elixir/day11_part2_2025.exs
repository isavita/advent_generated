
defmodule Main do
  def main do
    adj =
      File.stream!("input.txt")
      |> Stream.map(&String.trim/1)
      |> Enum.reduce(%{}, fn line, acc ->
        if line != "" do
          [src, rest] = String.split(line, ":", parts: 2)
          dests = String.split(rest, ~r/\s+/, trim: true)
          Map.put(acc, String.trim(src), dests)
        else
          acc
        end
      end)

    s1 = count_paths(adj, "svr", "dac") *
         count_paths(adj, "dac", "fft") *
         count_paths(adj, "fft", "out")

    s2 = count_paths(adj, "svr", "fft") *
         count_paths(adj, "fft", "dac") *
         count_paths(adj, "dac", "out")

    IO.puts("Paths (svr->dac->fft->out): #{s1}")
    IO.puts("Paths (svr->fft->dac->out): #{s2}")
    IO.puts("Total paths visiting both: #{s1 + s2}")
  end

  defp count_paths(adj, start, finish) do
    table = :ets.new(:memo, [:set, :private])
    result = dfs(adj, start, finish, table)
    :ets.delete(table)
    result
  end

  defp dfs(_adj, cur, cur, _table), do: 1

  defp dfs(adj, cur, finish, table) do
    case :ets.lookup(table, cur) do
      [{^cur, val}] -> val
      [] ->
        val =
          adj
          |> Map.get(cur, [])
          |> Enum.map(&dfs(adj, &1, finish, table))
          |> Enum.sum()

        :ets.insert(table, {cur, val})
        val
    end
  end
end

Main.main()
