
defmodule Solution do
  def parse_input(input) do
    input
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    line
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  defp all_zeros?(nums), do: Enum.all?(nums, &(&1 == 0))

  defp calculate_extrapolation(history) do
    history
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [a, b] -> b - a end)
  end

  defp calculate_extrapolations(history) do
    do_calculate_extrapolations([history])
  end

  defp do_calculate_extrapolations(extrapolations_series) do
    last = List.last(extrapolations_series)
    
    if all_zeros?(last) do
      extrapolations_series
    else
      new_extrapolation = calculate_extrapolation(last)
      do_calculate_extrapolations(extrapolations_series ++ [new_extrapolation])
    end
  end

  def solve(input) do
    input
    |> parse_input()
    |> Enum.map(fn history ->
      history
      |> calculate_extrapolations()
      |> Enum.reverse()
      |> Enum.reduce(0, fn series, acc -> 
        List.last(series) + acc 
      end)
    end)
    |> Enum.sum()
  end

  def run do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
    |> solve()
    |> IO.puts()
  end
end

Solution.run()
