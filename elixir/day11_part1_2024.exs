
defmodule StoneGame do
  def even_digits?(s) do
    String.length(s) |> rem(2) == 0
  end

  def trim_leading_zeros(s) do
    s = String.trim_leading(s, "0")
    if s == "" do
      "0"
    else
      s
    end
  end

  def process_stones(stones, 0), do: stones

  def process_stones(stones, n) do
    next_stones =
      Enum.flat_map(stones, fn s ->
        cond do
          s == "0" ->
            ["1"]
          even_digits?(s) ->
            mid = String.length(s) |> div(2)
            left = String.slice(s, 0..mid - 1) |> trim_leading_zeros()
            right = String.slice(s, mid..String.length(s) - 1) |> trim_leading_zeros()
            [left, right]
          true ->
            [Integer.to_string(String.to_integer(s) * 2024)]
        end
      end)

    process_stones(next_stones, n - 1)
  end

  def main do
    stones =
      File.read!("input.txt")
      |> String.trim()
      |> String.split()

    final_stones = process_stones(stones, 25)
    IO.puts(Enum.count(final_stones))
  end
end

StoneGame.main()
