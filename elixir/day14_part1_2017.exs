
defmodule Day14 do
  def solve() do
    input =
      "input.txt"
      |> File.read!()
      |> String.trim()

    0..127
    |> Enum.map(&knot_hash("#{input}-#{&1}"))
    |> Enum.map(&hex_to_binary/1)
    |> Enum.map(&String.graphemes/1)  #Split into chars
    |> List.flatten()
    |> Enum.count(fn x -> x == "1" end)
    |> IO.puts()
  end


  def knot_hash(input) do
    lengths =
      input
      |> String.to_charlist()
      |> Kernel.++([17, 31, 73, 47, 23])

    list = Enum.to_list(0..255)
    {dense_hash, _, _} =
      Enum.reduce(0..63, {list, 0, 0}, fn _, {current_list, current_position, skip_size} ->
        {new_list, new_position, new_skip_size} =
          Enum.reduce(lengths, {current_list, current_position, skip_size}, fn length,
                                                                                {list_acc, pos_acc, skip_acc} ->
            reversed_section =
              Enum.slice(list_acc, pos_acc, length) ++
                Enum.slice(list_acc, 0, max(0, pos_acc + length - length(list_acc)))

            reversed_section = Enum.reverse(reversed_section)

            updated_list =
              Enum.reduce(0..(length - 1), list_acc, fn i, acc ->
                List.replace_at(acc, rem(pos_acc + i, length(list_acc)), Enum.at(reversed_section, i))
              end)

            {updated_list, rem(pos_acc + length + skip_acc, length(list_acc)), skip_acc + 1}
          end)
          {new_list, new_position, new_skip_size}
      end)
    dense_hash
      |> Enum.chunk_every(16)
      |> Enum.map(fn chunk -> Enum.reduce(chunk, &Bitwise.bxor/2) end)
      |> Enum.map(&Integer.to_string(&1, 16))
      |> Enum.map(fn hex -> if String.length(hex) == 1, do: "0" <> hex, else: hex end) # Pad single digits
      |> Enum.join("")
  end


  def hex_to_binary(hex_string) do
    hex_string
    |> String.graphemes()
    |> Enum.map(fn hex_digit ->
      Integer.parse(hex_digit, 16)
      |> elem(0)
      |> Integer.to_string(2)
      |> String.pad_leading(4, "0") #Crucially pad leading zeros.
    end)
    |> Enum.join("")
  end
end

Day14.solve()
