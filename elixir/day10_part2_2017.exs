defmodule Day10Part2 do
  def call do
    input = read_input("input.txt")
    ascii_lengths = to_ascii_codes(input) ++ [17, 31, 73, 47, 23]
    {sparse_hash, _, _} = Enum.reduce(1..64, {Enum.to_list(0..255), 0, 0}, fn _, acc ->
      process_lengths(ascii_lengths, acc)
    end)
    dense_hash = create_dense_hash(sparse_hash)
    hex_string = Enum.map_join(dense_hash, &Integer.to_string(&1, 16) |> String.pad_leading(2, "0"))
    IO.puts(String.downcase(hex_string))
  end

  defp read_input(filename) do
    File.read!(filename) |> String.trim()
  end

  defp to_ascii_codes(input) do
    String.to_charlist(input) |> Enum.map(&(&1))
  end

  defp process_lengths(lengths, {list, current_pos, skip_size}) do
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

  defp create_dense_hash(sparse_hash) do
    Enum.chunk_every(sparse_hash, 16)
    |> Enum.map(fn chunk -> Enum.reduce(chunk, &Bitwise.bxor/2) end)
  end
end

Day10Part2.call()
