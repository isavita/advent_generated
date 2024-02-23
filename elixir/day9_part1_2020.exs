defmodule Day9 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
  end

  defp valid_sum?(target, preamble) do
    Enum.any?(preamble, fn x ->
      Enum.any?(preamble, fn y ->
        x != y and x + y == target
      end)
    end)
  end

  defp find_invalid_number(numbers, preamble_length) do
    numbers
    |> Enum.chunk_every(preamble_length + 1, 1, :discard)
    |> Enum.find(fn chunk ->
      preamble = Enum.take(chunk, preamble_length)
      target = List.last(chunk)
      not valid_sum?(target, preamble)
    end)
    |> List.last()
  end

  def call do
    numbers = read_input()
    preamble_length = 25
    invalid_number = find_invalid_number(numbers, preamble_length)
    IO.puts(invalid_number)
  end
end

Day9.call()
