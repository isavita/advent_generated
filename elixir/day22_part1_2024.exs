
defmodule Solution do
  def next_secret(s) do
    s = Bitwise.band(Bitwise.bxor(s, s * 64), 0xFFFFFF)
    s = Bitwise.band(Bitwise.bxor(s, div(s, 32)), 0xFFFFFF)
    Bitwise.band(Bitwise.bxor(s, s * 2048), 0xFFFFFF)
  end

  def solve(buyers) do
    buyers
    |> Enum.map(fn b ->
      Stream.iterate(b, &next_secret/1)
      |> Enum.at(2000)
    end)
    |> Enum.sum()
  end

  def main() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> solve()
    |> IO.puts()
  end
end

Solution.main()
