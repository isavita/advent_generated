defmodule Day10 do
  def call() do
    initial_sequence = File.read!("input.txt") |> String.trim()
    final_sequence = Enum.reduce(1..50, initial_sequence, fn _, acc -> 
      generate_next_sequence(acc)
    end)
    IO.puts(String.length(final_sequence))
  end

  defp generate_next_sequence(sequence) do
    sequence
    |> String.graphemes()
    |> Enum.chunk_by(& &1)
    |> Enum.map(&format_group/1)
    |> Enum.join("")
  end

  defp format_group(group) do
    "#{Enum.count(group)}#{Enum.at(group, 0)}"
  end
end

Day10.call()
