
defmodule Solution do
  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&find_digits/1)
    |> Enum.sum()
    |> IO.puts()
  end

  def find_digits(line) do
    digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    
    line
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.reduce({nil, nil}, fn {char, index}, {first, last} ->
      cond do
        Regex.match?(~r/\d/, char) ->
          digit = String.to_integer(char)
          {first || digit, digit}
        
        true ->
          matched_digit = Enum.find_index(digits, fn digit -> 
            String.starts_with?(String.slice(line, index..-1), digit) 
          end)
          
          case matched_digit do
            nil -> {first, last}
            digit -> {first || digit, digit}
          end
      end
    end)
    |> then(fn {first, last} -> first * 10 + last end)
  end
end

Solution.solve()
