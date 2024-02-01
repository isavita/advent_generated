
defmodule AdventCoinMiner do
  def call do
    input = File.read!("input.txt") |> String.trim()
    {mine_advent_coin(input, 5), mine_advent_coin(input, 6)}
  end

  defp mine_advent_coin(key, zeroes) do
    Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&{&1, :crypto.hash(:md5, key <> Integer.to_string(&1)) |> Base.encode16()})
    |> Stream.filter(&String.starts_with?(elem(&1, 1), String.duplicate("0", zeroes)))
    |> Enum.take(1)
    |> hd()
    |> elem(0)
  end
end
