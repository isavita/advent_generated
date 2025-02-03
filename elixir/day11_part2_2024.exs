
defmodule Solution do
  def trim_leading_zeros(s) do
    s
    |> String.trim_leading("0")
    |> then(&if(&1 == "", do: "0", else: &1))
  end

  def split_stone(s) do
    mid = div(String.length(s), 2)
    {trim_leading_zeros(String.slice(s, 0, mid)), trim_leading_zeros(String.slice(s, mid, String.length(s)))}
  end

  def multiply_by_2024(s) do
    num = String.to_integer(s)
    Integer.to_string(num * 2024)
  end

  def process(stones_map, 0) do
    stones_map
  end

  def process(stones_map, step) do
    new_stones_map =
      Enum.reduce(stones_map, %{}, fn {stone, count}, acc ->
        cond do
          stone == "0" ->
            Map.update(acc, "1", count, &(&1 + count))

          rem(String.length(stone), 2) == 0 ->
            {left, right} = split_stone(stone)
            acc
            |> Map.update(left, count, &(&1 + count))
            |> Map.update(right, count, &(&1 + count))

          true ->
            new_stone = multiply_by_2024(stone)
            Map.update(acc, new_stone, count, &(&1 + count))
        end
      end)
    process(new_stones_map, step - 1)
  end

  def main() do
    stones_str =
      "input.txt"
      |> File.read!()
      |> String.split()

    stones_map =
      stones_str
      |> Enum.reduce(%{}, fn s, acc -> Map.update(acc, s, 1, &(&1 + 1)) end)
      |> process(75)
      

    stones_map
    |> Map.values()
    |> Enum.sum()
    |> IO.puts()
  end
end

Solution.main()
