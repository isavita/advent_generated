defmodule Day17 do
  def call do
    steps = read_input("input.txt")
    result = spinlock(steps, 2017)
    
    IO.puts(result)
  end

  defp read_input(file_path) do
    {steps, _} = File.read!(file_path) |> String.trim() |> Integer.parse()
    steps
  end

  defp spinlock(steps, final_value) do
    process(0, steps, [0], 1, final_value)
  end

  defp process(_pos, _steps, buffer, current_value, final_value) when current_value > final_value do
    next_value_index = (Enum.find_index(buffer, &(&1 == final_value)) || 0) + 1
    Enum.at(buffer, next_value_index, "Buffer end reached")
  end

  defp process(pos, steps, buffer, current_value, final_value) do
    new_pos = rem(pos + steps, length(buffer)) + 1
    new_buffer = List.insert_at(buffer, new_pos, current_value)
    process(new_pos, steps, new_buffer, current_value + 1, final_value)
  end
end

Day17.call()
