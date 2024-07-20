
defmodule Extrapolation do
  def run do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> parse_input()
    |> solve()
    |> IO.puts()
  end

  defp parse_input(input) do
    Enum.map(input, fn line -> 
      line 
      |> String.split() 
      |> Enum.map(&String.to_integer/1) 
    end)
  end

  defp all_zeros(nums), do: Enum.all?(nums, &(&1 == 0))

  defp calculate_extrapolation(history) do
    Enum.zip(history, tl(history))
    |> Enum.map(fn {a, b} -> b - a end)
  end

  defp calculate_extrapolations(history) do
    Stream.iterate(history, &calculate_extrapolation/1)
    |> Enum.take_while(&!all_zeros(&1))
    |> Enum.to_list()
  end

  defp solve(histories) do
    Enum.reduce(histories, 0, fn history, acc ->
      extrapolations_series = calculate_extrapolations(history)
      past_prediction = 
        extrapolations_series 
        |> Enum.reverse() 
        |> Enum.reduce(0, fn series, past -> 
          hd(series) - past 
        end)

      acc + past_prediction
    end)
  end
end

Extrapolation.run()
