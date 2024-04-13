defmodule FFT do
  def main do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> apply_fft(100)
    |> Enum.take(8)
    |> Enum.join()
    |> IO.puts()
  end

  defp apply_fft(digits, 0), do: digits

  defp apply_fft(digits, phases) do
    digits
    |> do_fft()
    |> apply_fft(phases - 1)
  end

  defp do_fft(digits) do
    base_pattern = [0, 1, 0, -1]

    for i <- 0..(length(digits) - 1) do
      digits
      |> Enum.with_index()
      |> Enum.reduce(0, fn {x, j}, acc ->
        pattern_value = Enum.at(base_pattern, rem(div(j + 1, i + 1), length(base_pattern)))
        acc + x * pattern_value
      end)
      |> rem(10)
      |> Kernel.abs()  # Use Kernel.abs directly here
    end
  end
end

FFT.main()