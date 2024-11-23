
defmodule FlawedFrequencyTransmission do
  def solve do
    {:ok, input} = File.read("input.txt")
    input = String.trim(input)

    repeated_input = repeat_input(input, 10000)
    offset = String.slice(input, 0, 7) |> String.to_integer()

    result = Enum.reduce(1..100, repeated_input, fn _, acc -> process_phase(acc, offset) end)
    
    result
    |> Enum.slice(offset, 8)
    |> Enum.join()
    |> IO.puts()
  end

  defp repeat_input(input, times) do
    input
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> Stream.cycle()
    |> Enum.take(String.length(input) * times)
  end

  defp process_phase(digits, offset) do
    digits
    |> Enum.reverse()
    |> Enum.reduce({[], 0}, fn digit, {acc, sum} ->
      new_sum = sum + digit
      {[rem(new_sum, 10) | acc], new_sum}
    end)
    |> elem(0)
  end
end

FlawedFrequencyTransmission.solve()
