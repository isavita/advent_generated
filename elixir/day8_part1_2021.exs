defmodule DigitCounter do
  def run do
    "input.txt"
    |> File.stream!()
    |> Enum.reduce(0, fn line, acc ->
      [_, output] = String.split(line, " | ")
      output
      |> String.split()
      |> Enum.count(&unique_segment_count?/1)
      |> Kernel.+(acc)
    end)
    |> IO.puts()
  end

  defp unique_segment_count?(digit) do
    case String.length(digit) do
      2 -> true
      3 -> true
      4 -> true
      7 -> true
      _ -> false
    end
  end
end

DigitCounter.run()