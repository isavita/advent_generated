
defmodule TuningTrouble do
  def call do
    input = File.read!("input.txt")
    {packet_pos, message_pos} = detect_markers(input)
    {packet_pos, message_pos}
  end

  defp detect_markers(input) do
    packet_pos = detect_position(input, 4)
    message_pos = detect_position(input, 14)
    {packet_pos, message_pos}
  end

  defp detect_position(input, marker_length) do
    input
    |> String.graphemes()
    |> Enum.reduce_while({[], 0}, fn char, {acc, pos} ->
      updated_acc = [char | acc] |> Enum.take(marker_length)
      if Enum.uniq(updated_acc) == updated_acc and Enum.count(updated_acc) == marker_length do
        {:halt, pos + 1}
      else
        {:cont, {updated_acc, pos + 1}}
      end
    end)
  end
end
