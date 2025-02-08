
defmodule Day25 do
  def read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
  end

  def snafu_to_decimal(snafu) do
    snafu
    |> String.reverse()
    |> String.to_charlist()
    |> Enum.with_index()
    |> Enum.reduce(0, fn {char, index}, acc ->
      value =
        case char do
          ?= -> -2
          ?- -> -1
          ?0 -> 0
          ?1 -> 1
          ?2 -> 2
        end

      acc + value * :math.pow(5, index)
    end)
    |> round
  end
  
  def decimal_to_snafu(decimal) do
    if decimal == 0 do
      ""
    else
      remainder = rem(decimal, 5)
      next_decimal = div(decimal + 2, 5)

      case remainder do
        0 -> decimal_to_snafu(next_decimal) <> "0"
        1 -> decimal_to_snafu(next_decimal) <> "1"
        2 -> decimal_to_snafu(next_decimal) <> "2"
        3 -> decimal_to_snafu(next_decimal) <> "="
        4 -> decimal_to_snafu(next_decimal) <> "-"
      end
    end
  end

  def solve(filename) do
    input = read_input(filename)
    sum_decimal = Enum.reduce(input, 0, fn snafu, acc ->
      acc + snafu_to_decimal(snafu)
    end)

    decimal_to_snafu(sum_decimal)
  end
end

result = Day25.solve("input.txt")
IO.puts(result)
