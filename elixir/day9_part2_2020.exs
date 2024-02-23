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

  defp find_contiguous_set(numbers, target, start_index \\ 0)

  defp find_contiguous_set(numbers, target, start_index) do
    find_contiguous_set_helper(numbers, target, start_index, start_index, 0, [])
  end

  defp find_contiguous_set_helper(numbers, target, start_index, current_index, sum, acc) do
    case sum do
      ^target -> acc
      _ when sum > target or current_index == length(numbers) -> 
        find_contiguous_set(numbers, target, start_index + 1)
      _ ->
        new_sum = sum + Enum.at(numbers, current_index)
        new_acc = [Enum.at(numbers, current_index) | acc]
        find_contiguous_set_helper(numbers, target, start_index, current_index + 1, new_sum, new_acc)
    end
  end

  def call do
    numbers = read_input()
    preamble_length = 25
    invalid_number = find_invalid_number(numbers, preamble_length)
    contiguous_set = find_contiguous_set(numbers, invalid_number)
    |> Enum.sort()
    encryption_weakness = List.first(contiguous_set) + List.last(contiguous_set)
    IO.puts(encryption_weakness)
  end
end

Day9.call()
