
defmodule TileFlip do
  @directions %{
    "e" => {1, 0},
    "se" => {0, 1},
    "sw" => {-1, 1},
    "w" => {-1, 0},
    "nw" => {0, -1},
    "ne" => {1, -1}
  }

  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, black_tiles ->
      coord = parse_line(line)
      Map.update(black_tiles, coord, true, &(!&1))
    end)
    |> Map.values()
    |> Enum.count(&(&1))
    |> IO.puts()
  end

  defp parse_line(line) do
    line
    |> String.graphemes()
    |> parse_coordinates({0, 0})
  end

  defp parse_coordinates([], coord), do: coord
  defp parse_coordinates([h | t], {q, r}) do
    case h do
      "e" -> parse_coordinates(t, add_coord({q, r}, @directions["e"]))
      "w" -> parse_coordinates(t, add_coord({q, r}, @directions["w"]))
      "n" -> 
        [next | rest] = t
        dir = "n" <> next
        parse_coordinates(rest, add_coord({q, r}, @directions[dir]))
      "s" -> 
        [next | rest] = t
        dir = "s" <> next
        parse_coordinates(rest, add_coord({q, r}, @directions[dir]))
    end
  end

  defp add_coord({q1, r1}, {q2, r2}), do: {q1 + q2, r1 + r2}
end

TileFlip.solve()
