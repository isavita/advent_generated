defmodule Day10 do
  def call do
    lengths = read_input("input.txt")
    {list, _, _} = process_lengths(lengths, Enum.to_list(0..255), 0, 0)
    IO.puts Enum.at(list, 0) * Enum.at(list, 1)
  end

  defp read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  defp process_lengths(lengths, list, current_pos, skip_size) do
    Enum.reduce(lengths, {list, current_pos, skip_size}, fn length, {list, current_pos, skip_size} ->
      new_list = reverse_section(list, current_pos, length)
      new_pos = rem(current_pos + length + skip_size, length(list))
      {new_list, new_pos, skip_size + 1}
    end)
  end

  defp reverse_section(list, start_pos, length) do
    list_size = length(list)
    indices = Enum.map(0..length-1, &rem(&1 + start_pos, list_size))
    sublist = Enum.map(indices, &Enum.at(list, &1))
    reversed_sublist = Enum.reverse(sublist)

    Enum.with_index(reversed_sublist)
    |> Enum.reduce(list, fn {value, index}, acc ->
      replace_at_index(acc, Enum.at(indices, index), value)
    end)
  end

  defp replace_at_index(list, index, value) do
    Enum.with_index(list)
    |> Enum.map(fn {x, idx} -> if idx == index, do: value, else: x end)
  end
end

Day10.call()
