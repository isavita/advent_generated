defmodule Day1 do
  def call() do
    "input.txt"
    |> read_input
    |> parse_calories
    |> top_three_calories
    |> Enum.sum()
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

  defp top_three_calories(calories) do
    calories
    |> Enum.sort(&(&1 > &2))
    |> Enum.take(3)
  end
end

Day1.call()
