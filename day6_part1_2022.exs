
defmodule TuningTrouble do
  def call do
    input = File.read!("input.txt")
    find_marker(String.graphemes(input))
  end

  defp find_marker(graphemes) do
    find_marker(graphemes, 0)
  end

  defp find_marker([a, b, c, d | rest], index) when a != b and a != c and a != d and b != c and b != d and c != d do
    index + 4
  end

  defp find_marker([_ | rest], index) do
    find_marker(rest, index + 1)
  end
end
