
defmodule HexEd do
  def call do
    input = File.read!("input.txt") |> String.trim() |> String.split(",")
    {x, y} = Enum.reduce(input, {0, 0}, fn dir, {x, y} ->
      case dir do
        "n" -> {x, y + 1}
        "ne" -> {x + 1, y}
        "se" -> {x + 1, y - 1}
        "s" -> {x, y - 1}
        "sw" -> {x - 1, y}
        "nw" -> {x - 1, y + 1}
      end
    end)
    max(abs(x), abs(y))
  end
end
