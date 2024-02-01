
defmodule Day3 do
  def call do
    File.read!("input.txt")
    |> String.trim()
    |> String.graphemes()
    |> Enum.reduce({{0, 0}, %{}}, fn dir, {{x, y}, houses} ->
      new_pos = case dir do
        "^" -> {x, y + 1}
        "v" -> {x, y - 1}
        ">" -> {x + 1, y}
        "<" -> {x - 1, y}
      end
      {new_pos, Map.put(houses, new_pos, true)}
    end)
    |> elem(1)
    |> Map.size()
  end
end
