
defmodule Solution do
  def concat(a, b) do
    String.to_integer(Integer.to_string(a) <> Integer.to_string(b))
  end

  def can_produce(target, nums, idx, value) do
    cond do
      idx == length(nums) -> value == target
      true ->
        n = Enum.at(nums, idx)
        can_produce(target, nums, idx + 1, value + n) or
          can_produce(target, nums, idx + 1, value * n) or
          can_produce(target, nums, idx + 1, concat(value, n))
    end
  end

  def process_line(line) do
    case String.split(line, ":") do
      [target_str, nums_str] ->
        target = String.trim(target_str) |> String.to_integer()
        nums =
          String.split(String.trim(nums_str))
          |> Enum.map(&String.to_integer/1)

        case nums do
          [single_num] -> if single_num == target, do: target, else: 0
          _ -> if can_produce(target, nums, 1, hd(nums)), do: target, else: 0
        end

      _ -> 0
    end
  end

  def main do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&process_line/1)
    |> Enum.sum()
    |> IO.puts()
  end
end

Solution.main()
