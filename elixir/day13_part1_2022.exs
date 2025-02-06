
defmodule Day13 do
  def compare(left, right) when is_integer(left) and is_integer(right) do
    cond do
      left < right -> :lt
      left > right -> :gt
      true -> :eq
    end
  end

  def compare(left, right) when is_list(left) and is_integer(right) do
    compare(left, [right])
  end

  def compare(left, right) when is_integer(left) and is_list(right) do
    compare([left], right)
  end

  def compare(left, right) when is_list(left) and is_list(right) do
    compare_lists(left, right)
  end

  defp compare_lists([], []) do
    :eq
  end

  defp compare_lists([], _) do
    :lt
  end

  defp compare_lists(_, []) do
    :gt
  end

  defp compare_lists([h1 | t1], [h2 | t2]) do
    case compare(h1, h2) do
      :eq -> compare_lists(t1, t2)
      other -> other
    end
  end

  def parse_line(line) do
    line
    |> String.trim()
    |> Code.eval_string()
    |> elem(0)
  end

  def solve() do
    "input.txt"
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {pair_str, index}, acc ->
      [left_str, right_str] = String.split(pair_str, "\n", trim: true)
      left = parse_line(left_str)
      right = parse_line(right_str)

      if compare(left, right) == :lt do
        acc + index
      else
        acc
      end
    end)
    |> IO.inspect()
  end
end

Day13.solve()
