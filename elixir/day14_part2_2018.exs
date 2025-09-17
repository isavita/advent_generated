
defmodule Main do
  def main(_args) do
    input =
      "input.txt"
      |> File.read!()
      |> String.trim()

    digits =
      input
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)

    input_binary = :erlang.list_to_binary(digits)
    input_len = length(digits)

    scoreboard = <<3, 7>>
    final = loop(scoreboard, 0, 1, input_binary, input_len)

    IO.puts(byte_size(final) - input_len)
  end

  defp loop(scoreboard, elf1, elf2, input_binary, input_len) do
    left = :binary.at(scoreboard, elf1)
    right = :binary.at(scoreboard, elf2)
    sum = left + right

    if sum >= 10 do
      tens = div(sum, 10)
      sb1 = <<scoreboard::binary, tens>>
      if matches?(sb1, input_binary, input_len) do
        sb1
      else
        ones = rem(sum, 10)
        sb2 = <<sb1::binary, ones>>
        if matches?(sb2, input_binary, input_len) do
          sb2
        else
          len = byte_size(sb2)
          elf1n = rem(elf1 + left + 1, len)
          elf2n = rem(elf2 + right + 1, len)
          loop(sb2, elf1n, elf2n, input_binary, input_len)
        end
      end
    else
      ones = sum
      sb1 = <<scoreboard::binary, ones>>
      if matches?(sb1, input_binary, input_len) do
        sb1
      else
        len = byte_size(sb1)
        elf1n = rem(elf1 + left + 1, len)
        elf2n = rem(elf2 + right + 1, len)
        loop(sb1, elf1n, elf2n, input_binary, input_len)
      end
    end
  end

  defp matches?(scoreboard, input_binary, input_len) do
    total = byte_size(scoreboard)
    if total >= input_len do
      :binary.part(scoreboard, total - input_len, input_len) == input_binary
    else
      false
    end
  end
end

Main.main(System.argv())
