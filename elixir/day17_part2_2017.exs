defmodule Day17Part2 do
  def call do
    steps = read_input("input.txt")
    result = find_value_after_zero(steps, 50_000_000)
    
    IO.puts(result)
  end

  defp read_input(file_path) do
    {steps, _} = File.read!(file_path) |> String.trim() |> Integer.parse()
    steps
  end

  defp find_value_after_zero(steps, insertions) do
    Enum.reduce(1..insertions, {0, nil}, fn value, {pos, after_zero} ->
      new_pos = rem(pos + steps, value) + 1
      new_after_zero = if new_pos == 1, do: value, else: after_zero
      {new_pos, new_after_zero}
    end)
    |> elem(1)
  end
end

Day17Part2.call()
