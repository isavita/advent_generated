
defmodule CaptchaSolver do
  def call do
    input = File.read!("input.txt") |> String.trim()
    {part1(input), part2(input)}
  end

  defp part1(input) do
    input
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.filter(fn [a, b] -> a == b end)
    |> Enum.map(fn [a, _] -> a end)
    |> Enum.sum()
  end

  defp part2(input) do
    digits = String.graphemes(input) |> Enum.map(&String.to_integer/1)
    {first_half, second_half} = Enum.split(digits, div(length(digits), 2))
    combined = Enum.zip(first_half, second_half)

    combined
    |> Enum.filter(fn {a, b} -> a == b end)
    |> Enum.map(fn {a, _} -> a * 2 end)
    |> Enum.sum()
  end
end
