
defmodule Intcode do
  def call do
    input = File.read!("input.txt")
            |> String.trim()
            |> String.split(",")
            |> Enum.map(&String.to_integer/1)
    process(input, 0, 5)
  end

  defp process(codes, pointer, input, output \\ nil)

  defp process(codes, pointer, input, output) when is_list(codes) do
    {opcode, mode1, mode2, _mode3} = decode(codes, pointer)
    
    case opcode do
      1 -> process(apply_op(codes, pointer, mode1, mode2, &+/2), pointer + 4, input, output)
      2 -> process(apply_op(codes, pointer, mode1, mode2, &*/2), pointer + 4, input, output)
      3 -> process(update(codes, codes |> Enum.at(pointer + 1), input), pointer + 2, input, output)
      4 -> process(codes, pointer + 2, input, get_param(codes, pointer + 1, mode1))
      5 -> process(codes, jump_if(codes, pointer, mode1, mode2, &!=/2), input, output)
      6 -> process(codes, jump_if(codes, pointer, mode1, mode2, &==/2), input, output)
      7 -> process(apply_op(codes, pointer, mode1, mode2, &lt/2), pointer + 4, input, output)
      8 -> process(apply_op(codes, pointer, mode1, mode2, &eq/2), pointer + 4, input, output)
      99 -> output
    end
  end

  defp decode(codes, pointer) do
    opcode = Enum.at(codes, pointer) |> rem(100)
    mode1 = Enum.at(codes, pointer) |> div(100) |> rem(10)
    mode2 = Enum.at(codes, pointer) |> div(1000) |> rem(10)
    mode3 = Enum.at(codes, pointer) |> div(10000) |> rem(10)
    {opcode, mode1, mode2, mode3}
  end

  defp get_param(codes, index, mode) do
    case mode do
      0 -> Enum.at(codes, Enum.at(codes, index))
      1 -> Enum.at(codes, index)
    end
  end

  defp update(codes, index, value) do
    List.replace_at(codes, index, value)
  end

  defp apply_op(codes, pointer, mode1, mode2, op) do
    param1 = get_param(codes, pointer + 1, mode1)
    param2 = get_param(codes, pointer + 2, mode2)
    result = op.(param1, param2)
    update(codes, Enum.at(codes, pointer + 3), result)
  end

  defp jump_if(codes, pointer, mode1, mode2, condition) do
    param1 = get_param(codes, pointer + 1, mode1)
    param2 = get_param(codes, pointer + 2, mode2)
    if condition.(param1, 0), do: param2, else: pointer + 3
  end

  defp lt(a, b), do: if a < b, do: 1, else: 0
  defp eq(a, b), do: if a == b, do: 1, else: 0
end
