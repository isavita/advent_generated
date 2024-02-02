
defmodule PasswordPhilosophy do
  def call do
    input = File.read!("input.txt")
    |> String.split("\n", trim: true)

    count_valid_passwords(input)
  end

  defp count_valid_passwords(input) do
    Enum.count(input, fn line ->
      [policy, password] = String.split(line, ": ")
      [positions, letter] = String.split(policy, " ")
      [pos1, pos2] = String.split(positions, "-") |> Enum.map(&String.to_integer/1)

      (String.at(password, pos1 - 1) == letter) != (String.at(password, pos2 - 1) == letter)
    end)
  end
end
