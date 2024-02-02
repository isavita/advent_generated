
defmodule Day4 do
  def call do
    input = File.read!("input.txt")
    [start, stop] = String.split(input, "-") |> Enum.map(&String.to_integer/1)
    count_valid_passwords(start, stop)
  end

  defp count_valid_passwords(start, stop) do
    Enum.count(start..stop, &is_valid_password/1)
  end

  defp is_valid_password(password) do
    digits = Integer.digits(password)
    has_adjacent_digits(digits) && never_decreases(digits)
  end

  defp has_adjacent_digits([a, b | _]) when a == b, do: true
  defp has_adjacent_digits([_ | rest]), do: has_adjacent_digits(rest)
  defp has_adjacent_digits(_), do: false

  defp never_decreases([a, b | rest]) when a > b, do: false
  defp never_decreases([_ | rest]), do: never_decreases(rest)
  defp never_decreases(_), do: true
end
