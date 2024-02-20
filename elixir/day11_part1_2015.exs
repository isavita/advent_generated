defmodule Day11 do
  def call() do
    current_password = File.read!("input.txt") |> String.trim()
    next_password = find_next_valid_password(current_password)
    IO.puts(next_password)
  end

  defp find_next_valid_password(password) do
    Stream.iterate(password, &increment_password/1)
    |> Enum.find(&valid_password?/1)
  end

  defp increment_password(password) do
    password
    |> String.to_charlist()
    |> Enum.reverse()
    |> increment_charlist()
    |> Enum.reverse()
    |> List.to_string()
  end

  defp increment_charlist([122 | t]), do: [97 | increment_charlist(t)] # 'z' to 'a', carry the increment
  defp increment_charlist([h | t]) when h < 122, do: [h + 1 | t] # Increment char
  defp increment_charlist([]), do: ['a'] # Handle empty, start with 'a'

  defp valid_password?(password) do
    has_straight?(password) &&
      !Regex.match?(~r/[iol]/, password) && # Corrected to use Regex.match?/2
      has_two_pairs?(password)
  end

  defp has_straight?(password) do
    password
    |> String.to_charlist()
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.any?(fn [a, b, c] -> b - a == 1 && c - b == 1 end)
  end

  defp has_two_pairs?(password) do
    pairs = Regex.scan(~r/(.)\1/, password) |> Enum.uniq()
    length(pairs) >= 2
  end
end

Day11.call()
