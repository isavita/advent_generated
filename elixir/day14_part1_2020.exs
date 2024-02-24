defmodule Day14 do
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

  defp apply_mask(value, mask) do
    value_binary = Integer.to_string(value, 2) |> String.pad_leading(36, "0")

    Enum.zip(String.graphemes(value_binary), String.graphemes(mask))
    |> Enum.map(fn {v, m} -> if m == "X", do: v, else: m end)
    |> Enum.join("")
    |> String.to_integer(2)
  end

  defp process_instructions(instructions) do
    Enum.reduce(instructions, {%{}, ""}, fn
      {:mask, mask}, {mem, _} ->
        {mem, mask}

      {:mem, address, value}, {mem, mask} ->
        new_value = apply_mask(value, mask)
        {Map.put(mem, address, new_value), mask}
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

Day14.call()
