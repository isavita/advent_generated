
defmodule Day20 do
  def solve(filename) do
    input = File.read!(filename) |> String.trim() |> String.to_integer()
    target = div(input, 10)

    houses =
      Stream.unfold(1, fn elf ->
        if elf <= target do
          {{elf, elf}, elf + 1}
        else
          nil
        end
      end)
      |> Enum.reduce(%{}, fn {elf, house}, acc ->
        Stream.iterate(house, &(&1 + elf))
        |> Stream.take_while(fn h -> h <= target end)
        |> Enum.reduce(acc, fn h, acc2 ->
          Map.update(acc2, h, elf, &(&1 + elf))
        end)
      end)

    Enum.find(1..target, fn house ->
      Map.get(houses, house, 0) >= target
    end)
  end
end

filename = "input.txt"
result = Day20.solve(filename)
IO.puts(result)
