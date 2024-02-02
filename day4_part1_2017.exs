
defmodule Passphrase do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    valid_passphrases = input |> Enum.filter(&is_valid_passphrase/1)
    length(valid_passphrases)
  end

  defp is_valid_passphrase(passphrase) do
    passphrase
    |> String.split(" ")
    |> Enum.uniq()
    |> length() == String.split(passphrase, " ") |> length()
  end
end
