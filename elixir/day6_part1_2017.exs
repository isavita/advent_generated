defmodule SignalsAndNoise do
  def read_input do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
  end

  def transpose(messages) do
    messages
    |> Enum.map(&String.graphemes/1)
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)  # Convert each tuple into a list
  end

  def most_common_char(column) do
    column
    |> Enum.frequencies()
    |> Enum.max_by(fn {_char, freq} -> freq end)
    |> elem(0)
  end

  def error_corrected_message(messages) do
    messages
    |> transpose()
    |> Enum.map(&most_common_char/1)
    |> Enum.join()
  end

  def run do
    read_input()
    |> error_corrected_message()
    |> IO.puts()
  end
end

SignalsAndNoise.run()
