
defmodule Day4 do
  def call do
    input = File.read!("input.txt") |> String.split("\n") |> List.first |> String.split("-") |> Enum.map(&String.to_integer/1)
    Range.new(input |> List.first, input |> List.last)
    |> Enum.filter(&valid_password/1)
    |> Enum.count()
  end

  defp valid_password(password) do
    digits = Integer.digits(password)
    has_adjacent_digits(digits) && never_decreases(digits) && has_exact_double(digits)
  end

  defp has_adjacent_digits([a, b | _]) when a == b, do: true
  defp has_adjacent_digits([_ | rest]), do: has_adjacent_digits(rest)
  defp has_adjacent_digits(_), do: false

  defp never_decreases([a, b | rest]) when a > b, do: false
  defp never_decreases([_ | rest]), do: never_decreases(rest)
  defp never_decreases(_), do: true

  defp has_exact_double(digits) do
    Enum.reduce(digits, %{}, fn digit, acc ->
      Map.update(acc, digit, 1, &(&1 + 1))
    end)
    |> Map.values()
    |> Enum.any?(&(&1 == 2))
  end
end
