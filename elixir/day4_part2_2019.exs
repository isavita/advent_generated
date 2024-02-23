defmodule Day4 do
  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("-")
    |> Enum.map(&String.to_integer/1)
  end

  def valid_password?(password) do
    digits = Integer.digits(password)
    has_double = Enum.chunk_by(digits, & &1) |> Enum.any?(fn group -> Enum.count(group) == 2 end)
    never_decreases = digits |> Enum.chunk_every(2, 1, :discard) |> Enum.all?(fn [a, b] -> a <= b end)
    has_double and never_decreases
  end

  def count_valid_passwords(range) do
    Enum.count(range, &valid_password?/1)
  end

  def call do
    [start_range, end_range] = read_input()
    range = start_range..end_range
    valid_passwords_count = count_valid_passwords(range)
    IO.puts(valid_passwords_count)
  end
end

Day4.call()
