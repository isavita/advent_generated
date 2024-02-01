
defmodule PassportProcessing do
  def call do
    File.read!("input.txt")
    |> String.split("\n\n", trim: true)
    |> Enum.count(&is_valid_passport/1)
  end

  def is_valid_passport(passport) do
    fields = String.split(passport, ~r/\s+/)
    required_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    Enum.all?(required_fields, &has_field(&1, fields)) && Enum.all?(fields, &is_valid_field/1)
  end

  def has_field(field, fields) do
    Enum.any?(fields, &String.starts_with?(&1, field))
  end

  def is_valid_field(field) do
    [key, value] = String.split(field, ":")
    case key do
      "byr" -> String.match?(value, ~r/^\d{4}$/) && value >= "1920" && value <= "2002"
      "iyr" -> String.match?(value, ~r/^\d{4}$/) && value >= "2010" && value <= "2020"
      "eyr" -> String.match?(value, ~r/^\d{4}$/) && value >= "2020" && value <= "2030"
      "hgt" -> case Regex.scan(~r/(\d+)(cm|in)/, value) do
                 [[_, height, unit]] -> (unit == "cm" && height >= "150" && height <= "193") || (unit == "in" && height >= "59" && height <= "76")
                 _ -> false
               end
      "hcl" -> String.match?(value, ~r/^#[0-9a-f]{6}$/)
      "ecl" -> value in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      "pid" -> String.match?(value, ~r/^\d{9}$/)
      _ -> true
    end
  end
end
