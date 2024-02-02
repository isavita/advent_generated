
defmodule Diagnostic do
  def call do
    input = File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes(&1))

    {gamma, epsilon} = Enum.reduce(0..Enum.count(hd(input)) - 1, {"", ""}, fn index, {gamma_acc, epsilon_acc} ->
      {zeros, ones} = Enum.reduce(input, {0, 0}, fn number, {z, o} ->
        if Enum.at(number, index) == "0", do: {z + 1, o}, else: {z, o + 1}
      end)

      gamma_bit = if zeros > ones, do: "0", else: "1"
      epsilon_bit = if zeros > ones, do: "1", else: "0"

      {gamma_acc <> gamma_bit, epsilon_acc <> epsilon_bit}
    end)

    String.to_integer(gamma, 2) * String.to_integer(epsilon, 2)
  end
end
