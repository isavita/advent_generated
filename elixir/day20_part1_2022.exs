
defmodule Day20 do
  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
  end

  def mix(numbers) do
    length = length(numbers)

    Enum.reduce(numbers, numbers, fn {value, original_index}, acc ->
      current_index = Enum.find_index(acc, fn {_, oi} -> oi == original_index end)

      new_index = rem(current_index + value, length - 1)
      new_index = if new_index < 0, do: new_index + length - 1, else: new_index

      acc
      |> List.delete_at(current_index)
      |> List.insert_at(new_index, {value, original_index})
    end)
  end

  def grove_coordinates(mixed_numbers) do
    length = length(mixed_numbers)
    zero_index = Enum.find_index(mixed_numbers, fn {val, _} -> val == 0 end)

    [1000, 2000, 3000]
    |> Enum.map(fn offset ->
      index = rem(zero_index + offset, length)
      {value, _} = Enum.at(mixed_numbers, index)
      value
    end)
    |> Enum.sum()
  end

  def solve(filename) do
    numbers = read_input(filename)
    mixed_numbers = mix(numbers)
    grove_coordinates(mixed_numbers)
  end
end

result = Day20.solve("input.txt")
IO.puts(result)
