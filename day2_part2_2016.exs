
defmodule BathroomSecurity do
  def call do
    input = File.read!("input.txt")
    {code1, _} = Enum.reduce(input |> String.split("\n"), {"", 5}, &process_line/2)
    {code2, _} = Enum.reduce(input |> String.split("\n"), {"", 5}, &process_line_complex/2)
    {code1, code2}
  end

  defp process_line(line, {code, pos}) do
    new_pos = Enum.reduce(String.graphemes(line), pos, &move/2)
    {code <> Integer.to_string(new_pos), new_pos}
  end

  defp move("U", pos), do: if pos > 3, do: pos - 3, else: pos
  defp move("D", pos), do: if pos < 7, do: pos + 3, else: pos
  defp move("L", pos), do: if rem(pos - 1, 3) > 0, do: pos - 1, else: pos
  defp move("R", pos), do: if rem(pos, 3) > 0, do: pos + 1, else: pos

  defp process_line_complex(line, {code, pos}) do
    new_pos = Enum.reduce(String.graphemes(line), pos, &move_complex/2)
    {code <> to_hex(new_pos), new_pos}
  end

  defp move_complex("U", pos), do: Map.get(%{5 => 5, 2 => 2, 1 => 1, 4 => 4, 9 => 9, 6 => 2, 7 => 3, 8 => 4, 3 => 1, 10 => 6, 11 => 7, 12 => 8, 13 => 11}, pos, pos)
  defp move_complex("D", pos), do: Map.get(%{5 => 5, 2 => 6, 1 => 3, 4 => 8, 9 => 9, 6 => 10, 7 => 11, 8 => 12, 3 => 7, 10 => 10, 11 => 13, 12 => 12, 13 => 13}, pos, pos)
  defp move_complex("L", pos), do: Map.get(%{1 => 1, 2 => 2, 3 => 2, 4 => 3, 5 => 5, 6 => 5, 7 => 6, 8 => 7, 9 => 8, 10 => 10, 11 => 10, 12 => 11, 13 => 13}, pos, pos)
  defp move_complex("R", pos), do: Map.get(%{1 => 1, 2 => 3, 3 => 4, 4 => 4, 5 => 6, 6 => 7, 7 => 8, 8 => 9, 9 => 9, 10 => 11, 11 => 12, 12 => 12, 13 => 13}, pos, pos)

  defp to_hex(pos) do
    case pos do
      10 -> "A"
      11 -> "B"
      12 -> "C"
      13 -> "D"
      _ -> Integer.to_string(pos)
    end
  end
end
