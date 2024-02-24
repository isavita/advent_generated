defmodule Day14Part2 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction("mask = " <> mask) do
    {:mask, mask}
  end

  defp parse_instruction(instruction) do
    [mem, value] = Regex.run(~r/mem\[(\d+)\] = (\d+)/, instruction, capture: :all_but_first)
    {:mem, String.to_integer(mem), String.to_integer(value)}
  end

  defp apply_mask_to_address(address, mask) do
    address_binary = Integer.to_string(address, 2) |> String.pad_leading(36, "0")

    Enum.zip_with(String.graphemes(address_binary), String.graphemes(mask), fn a, m ->
      case m do
        "0" -> a
        "1" -> "1"
        "X" -> "X"
      end
    end)
    |> Enum.join("")
  end

  defp generate_addresses(masked_address) do
    get_addresses([masked_address], [])
  end

  defp get_addresses([], acc), do: acc

  defp get_addresses([address | rest], acc) do
    case String.contains?(address, "X") do
      true ->
        address1 = String.replace(address, "X", "0", global: false)
        address2 = String.replace(address, "X", "1", global: false)
        get_addresses([address1, address2 | rest], acc)

      false ->
        get_addresses(rest, [String.to_integer(address, 2) | acc])
    end
  end

  defp process_instructions(instructions) do
    Enum.reduce(instructions, {%{}, ""}, fn
      {:mask, mask}, {mem, _} ->
        {mem, mask}

      {:mem, address, value}, {mem, mask} ->
        masked_address = apply_mask_to_address(address, mask)
        addresses = generate_addresses(masked_address)
        new_mem = Enum.reduce(addresses, mem, fn addr, acc -> Map.put(acc, addr, value) end)
        {new_mem, mask}
    end)
    |> elem(0)
  end

  def call do
    instructions = read_input()
    memory = process_instructions(instructions)
    sum = Enum.reduce(memory, 0, fn {_k, v}, acc -> acc + v end)
    IO.puts(sum)
  end
end

Day14Part2.call()
