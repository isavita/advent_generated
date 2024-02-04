defmodule Task do
  @limit 272

  def parse_input(input) do
       input
      |> String.graphemes
      |> Enum.map(&String.to_integer/1)
  end

  def calculate(input) do
      input
      |> parse_input()
      |> generate(@limit)
      |> checksum
  end

  def checksum(input) when rem(length(input), 2) == 1 do
    IO.puts "YAY, #{Enum.join(input)}!"
  end
  def checksum(input) do
    input
      |> Enum.chunk(2)
      |> Enum.map(&chesum_pair/1)
      |> checksum
  end

  def chesum_pair([a, b]) when a == b, do: 1
  def chesum_pair([_a, _b]), do: 0

  def generate(input, limit) when length(input) >= limit do
    Enum.slice(input, 0, limit)
  end
  def generate(input, limit) do
    input = input ++ [0] ++ reverse(input)
    generate(input, limit)
  end

  def reverse(input) do
    input
      |> Enum.reverse
      |> Enum.map(&toggle/1)
  end

  def toggle(1), do: 0
  def toggle(0), do: 1
end

input = File.read!("input.txt")
result = Task.calculate(input)
IO.inspect result
