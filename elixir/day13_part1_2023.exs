
defmodule Day13 do
  def solve(input_file \\ "input.txt") do
    input_file
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.map(&parse_pattern/1)
    |> Enum.map(&find_reflection/1)
    |> Enum.sum()
    |> IO.puts()
  end

  defp parse_pattern(pattern_str) do
    pattern_str
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  defp find_reflection(pattern) do
    case find_vertical_reflection(pattern) do
      {:ok, cols} -> cols
      :error ->
        case find_horizontal_reflection(pattern) do
          {:ok, rows} -> rows * 100
          :error -> raise "No reflection found"
        end
    end
  end

  defp find_vertical_reflection(pattern) do
    num_cols = length(hd(pattern))
    1..(num_cols - 1)
    |> Enum.find_value(fn cols_left ->
      if is_vertical_reflection?(pattern, cols_left) do
        {:ok, cols_left}
      else
        nil
      end
    end) || :error
  end

  defp is_vertical_reflection?(pattern, cols_left) do
      pattern
      |> Enum.all?(fn row ->
        left = Enum.take(row, cols_left) |> Enum.reverse()
        right = Enum.drop(row, cols_left)
        min_len = min(length(left), length(right))
        Enum.take(left, min_len) == Enum.take(right, min_len)

      end)
  end

  defp find_horizontal_reflection(pattern) do
    transposed_pattern = transpose(pattern)
    find_vertical_reflection(transposed_pattern)
  end

  defp transpose(matrix) do
    # More concise and efficient transpose using zip
    matrix
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end
end

Day13.solve()
