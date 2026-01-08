
defmodule Main do
  def main do
    total =
      "input.txt"
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Stream.filter(&(&1 != ""))
      |> Enum.map(&calculate_max_joltage/1)
      |> Enum.sum()

    IO.puts("Total output joltage: #{total}")
  end

  defp calculate_max_joltage(bank) do
    Enum.reduce_while(9..0, 0, fn d1, _acc ->
      ch = <<?0 + d1>>
      case :binary.match(bank, ch) do
        {idx, _len} ->
          rest = binary_part(bank, idx + 1, byte_size(bank) - idx - 1)

          max_d2 =
            rest
            |> :binary.bin_to_list()
            |> Enum.filter(&(&1 in ?0..?9))
            |> Enum.map(&(&1 - ?0))
            |> Enum.max(fn -> -1 end)

          if max_d2 != -1 do
            {:halt, d1 * 10 + max_d2}
          else
            {:cont, 0}
          end

        :nomatch ->
          {:cont, 0}
      end
    end)
  end
end

Main.main()
