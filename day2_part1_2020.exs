
defmodule PasswordPhilosophy do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.count(&valid_password?/1)
  end

  defp parse_line(line) do
    [range, letter, password] = Regex.split(~r/:?\s+/, line)
    {min, max} = range |> String.split("-") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
    {min, max, String.first(letter), password}
  end

  defp valid_password?({min, max, letter, password}) do
    count = String.codepoints(password) |> Enum.filter(&( &1 == to_string([letter]) )) |> length()
    count >= min and count <= max
  end
end
