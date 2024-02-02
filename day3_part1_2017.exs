
defmodule SpiralMemory do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.to_integer()
    find_steps(input)
  end

  defp find_steps(n) do
    root = :math.sqrt(n) |> Float.floor() |> trunc()
    root = if rem(root, 2) == 0, do: root - 1, else: root
    layer = div(root, 2)
    leg_len = root + 1
    max_val = root * root
    steps_to_corner = leg_len - 1
    offset = rem(n - max_val - 1, leg_len)
    layer + abs(div(leg_len, 2) - offset)
  end
end
