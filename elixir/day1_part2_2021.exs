
defmodule SonarSweep do
  def call do
    input = File.read!("input.txt") |> String.split("\n") |> Enum.map(&String.to_integer/1)
    {count_increases(input), count_increasing_sums(input)}
  end

  defp count_increases(input) do
    Enum.reduce(input, {0, nil}, fn depth, {count, prev} ->
      case prev do
        nil -> {count, depth}
        _ when depth > prev -> {count + 1, depth}
        _ -> {count, depth}
      end
    end) |> elem(0)
  end

  defp count_increasing_sums(input) do
    Enum.chunk_every(input, 3, 1, :discard) 
    |> Enum.map(&Enum.sum(&1))
    |> count_increases()
  end
end
