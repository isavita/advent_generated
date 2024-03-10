defmodule Decompressor do
  def solve do
    # Read input from file
    input = read_input()

    # Calculate the decompressed length
    decompressed_length = calculate_decompressed_length(input)

    # Print the result
    IO.puts("Decompressed length: #{decompressed_length}")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
  end

  defp calculate_decompressed_length(input) do
    input
    |> String.graphemes()
    |> do_calculate_decompressed_length()
  end

  defp do_calculate_decompressed_length([]), do: 0

  defp do_calculate_decompressed_length(["(" | rest]) do
    {marker, remaining} = parse_marker(rest)
    {data_length, repeat_times} = parse_marker_values(marker)

    {data, remaining} = Enum.split(remaining, data_length)
    decompressed_data_length = do_calculate_decompressed_length(data)
    decompressed_length = decompressed_data_length * repeat_times

    decompressed_length + do_calculate_decompressed_length(remaining)
  end

  defp do_calculate_decompressed_length([_ | rest]) do
    1 + do_calculate_decompressed_length(rest)
  end

  defp parse_marker(chars) do
    do_parse_marker(chars, [])
  end

  defp do_parse_marker([")" | rest], acc), do: {Enum.reverse(acc), rest}

  defp do_parse_marker([char | rest], acc) do
    do_parse_marker(rest, [char | acc])
  end

  defp parse_marker_values(marker) do
    [data_length, repeat_times] =
      marker
      |> Enum.join()
      |> String.split("x")
      |> Enum.map(&String.to_integer/1)

    {data_length, repeat_times}
  end
end

Decompressor.solve()
