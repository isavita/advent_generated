defmodule Day25 do
  def call() do
    input = File.read!("input.txt")
    {row, col} = parse_input(input)
    sequence_number = calculate_sequence_number(row, col)
    code = calculate_code(sequence_number)
    IO.puts(code)
  end

  defp parse_input(input) do
    Regex.run(~r/row (\d+), column (\d+)/, input, capture: :all_but_first)
    |> Enum.map(&String.to_integer/1)
    |> case do
      [row, col] -> {row, col}
      _ -> {0, 0} # Default case if parsing fails
    end
  end

  defp calculate_sequence_number(row, col) do
    diagonal = row + col - 1
    total_previous = div(diagonal * (diagonal - 1), 2)
    total_previous + col
  end

  defp calculate_code(sequence_number) do
    start_code = 20151125
    multiplier = 252533
    modulus = 33554393

    Enum.reduce(1..sequence_number - 1, start_code, fn _, acc ->
      rem(acc * multiplier, modulus)
    end)
  end
end

Day25.call()
