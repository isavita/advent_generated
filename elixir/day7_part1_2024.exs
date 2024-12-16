
defmodule Solution do
  def can_produce(target, nums, idx, current) do
    case idx == length(nums) do
      true -> current == target
      false ->
        plus = can_produce(target, nums, idx + 1, current + Enum.at(nums, idx))
        mul = can_produce(target, nums, idx + 1, current * Enum.at(nums, idx))
        plus or mul
    end
  end

  def process_line(line) do
    case line do
      "" -> 0
      _ ->
        [target_str, nums_str] = String.split(line, ":", parts: 2)
        target = String.trim(target_str) |> String.to_integer()
        nums =
          String.split(String.trim(nums_str))
          |> Enum.map(&String.to_integer/1)

        case length(nums) do
          1 -> if Enum.at(nums, 0) == target, do: target, else: 0
          _ -> if can_produce(target, nums, 1, Enum.at(nums, 0)), do: target, else: 0
        end
    end
  end

  def main do
    File.read!("input.txt")
    |> String.split("\n")
    |> Enum.map(&process_line/1)
    |> Enum.sum()
    |> IO.puts()
  end
end

Solution.main()
