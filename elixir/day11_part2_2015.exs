defmodule Day11 do
  def call() do
    initial_password = File.read!("input.txt") |> String.trim()
    first_new_password = find_next_valid_password(initial_password)
    # Start from the next password after the first new password
    second_new_password = first_new_password |> increment_password() |> find_next_valid_password()
    IO.puts(second_new_password)
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
      !Regex.match?(~r/[iol]/, password) &&
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
