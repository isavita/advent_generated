
defmodule Solver do
  def solve(filename) do
    filename
    |> File.stream!()
    |> Stream.map(&parse_levels/1)
    |> Stream.filter(&(&1 != nil))
    |> Stream.filter(&(is_safe_report?(&1) || is_safe_with_one_removal?(&1)))
    |> Enum.count()
    |> IO.puts()
  end

  defp parse_levels(line) do
    line
    |> String.trim()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
    |> case do
      levels when is_list(levels) -> levels
      _ -> nil
    end
  end

  defp is_safe_report?(levels) do
    case levels do
      [_] -> false
      [a, b | _] ->
        diff = b - a
        if diff == 0 do
          false
        else
          (diff > 0 && is_increasing?(levels, abs(diff))) ||
          (diff < 0 && is_decreasing?(levels, abs(diff)))
        end

      _ ->
        false
    end
  end

  defp is_increasing?([_], _), do: true
  defp is_increasing?([a, b | rest], prev_diff) do
    diff = b - a
    abs_diff = abs(diff)
    if diff > 0 and abs_diff >= 1 and abs_diff <= 3 do
      is_increasing?([b | rest], abs_diff)
    else
      false
    end
  end

  defp is_decreasing?([_], _), do: true
  defp is_decreasing?([a, b | rest], prev_diff) do
    diff = b - a
    abs_diff = abs(diff)
    if diff < 0 and abs_diff >= 1 and abs_diff <= 3 do
      is_decreasing?([b | rest], abs_diff)
    else
      false
    end
  end

  defp is_safe_with_one_removal?(levels) do
    0..(length(levels) - 1)
    |> Enum.any?(fn i ->
      levels
      |> List.delete_at(i)
      |> is_safe_report?()
    end)
  end
end

Solver.solve("input.txt")
