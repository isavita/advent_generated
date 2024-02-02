defmodule Day4 do
  def call do
    input = File.read!("input.txt")
    |> String.split("\n\n", trim: true)

    count_valid_passports(input)
  end

  defp count_valid_passports(input) do
    input
    |> Enum.count(&is_valid_passport/1)
  end

  defp is_valid_passport(passport) do
    required_fields = ~w(byr iyr eyr hgt hcl ecl pid)
    fields = passport
             |> String.split(~r/\s+/)
             |> Enum.map(&String.split(&1, ":"))
             |> Enum.map(fn [key, _value] -> key end)

    Enum.all?(required_fields, &(&1 in fields))
  end
end