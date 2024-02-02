
defmodule FirewallRules do
  def call do
    input = File.read!("input.txt")
    |> String.split("\n")
    |> Enum.map(&String.split(&1, "-"))
    |> Enum.map(&{String.to_integer(Enum.at(&1, 0)), String.to_integer(Enum.at(&1, 1))})
    |> Enum.sort()

    Enum.reduce(input, 0, fn {start, _end}, acc ->
      if acc < start do
        acc
      else
        _end + 1
      end
    end)
  end
end
