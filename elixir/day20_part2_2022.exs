
defmodule Day20 do
  def read_input(path) do
    File.read!(path)
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.map(fn {s, i} -> {i, String.to_integer(s)} end)
  end

  def mix(nums) do
    n = length(nums) - 1
    Enum.reduce(0..length(nums) - 1, nums, fn i, acc ->
      {old_pos, val} = Enum.at(acc, i)
      new_pos = rem(old_pos + val, n)
      new_pos = if new_pos < 0, do: new_pos + n, else: new_pos

      updated_nums =
        Enum.map(acc, fn {pos, v} ->
          cond do
            pos > old_pos and pos <= new_pos -> {pos - 1, v}
            pos >= new_pos and pos < old_pos -> {pos + 1, v}
            true -> {pos, v}
          end
        end)

      List.replace_at(updated_nums, i, {new_pos, val})
    end)
  end

  def coords(nums) do
    l = length(nums)
    zero_pos = Enum.find(nums, fn {_, v} -> v == 0 end) |> elem(0)
    Enum.reduce(nums, 0, fn {pos, val}, acc ->
      cond do
        pos == rem(zero_pos + 1000, l) or
          pos == rem(zero_pos + 2000, l) or
          pos == rem(zero_pos + 3000, l) -> acc + val
        true -> acc
      end
    end)
  end

  def solve(path) do
    nums = read_input(path)
    nums2 = Enum.map(nums, fn {pos, val} -> {pos, val * 811_589_153} end)
    nums2 = Enum.reduce(1..10, nums2, fn _, acc -> mix(acc) end)
    coords(nums2)
  end
end

IO.puts(Day20.solve("input.txt"))
