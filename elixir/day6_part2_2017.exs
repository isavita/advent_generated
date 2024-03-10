defmodule ErrorCorrector do
  def call do
    input = read_input()

    # Decode the message
    decoded_message = decode_message(input)

    # Print the result
    IO.puts(decoded_message)
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  defp decode_message(input) do
    input
    |> Enum.map(&String.graphemes/1)
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&find_least_common_character/1)
    |> Enum.join()
  end

  defp find_least_common_character(characters) do
    characters
    |> Enum.frequencies()
    |> Enum.min_by(fn {_char, frequency} -> frequency end)
    |> elem(0)
  end
end

ErrorCorrector.call()
