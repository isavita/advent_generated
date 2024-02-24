defmodule Day1 do
  def call() do
    "input.txt"
    |> read_input
    |> parse_calories
    |> max_calories
    |> IO.puts()
  end

  defp read_input(file_path) do
    File.read!(file_path)
    |> String.trim()
  end

  defp parse_calories(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(&calculate_sum/1)
  end

  defp calculate_sum(block) do
    block
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  defp max_calories(calories) do
    Enum.max(calories)
  end
end

Day1.call()
