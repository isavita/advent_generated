
File.read!("input.txt")
|> String.trim()
|> String.split(~r/mul\(\d{1,3},\d{1,3}\)/, include_captures: true, trim: true)
|> Enum.reduce(0, fn segment, acc ->
  case Regex.run(~r/mul\((\d{1,3}),(\d{1,3})\)/, segment) do
    [_, a, b] ->
      acc + String.to_integer(a) * String.to_integer(b)

    _ ->
      acc
  end
end)
|> IO.puts()
