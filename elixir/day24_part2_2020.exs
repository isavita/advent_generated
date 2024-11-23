
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
    black_tiles = 
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.reduce(%{}, fn line, acc -> 
        coord = parse_line(line)
        Map.update(acc, coord, true, &(!&1))
      end)

    Stream.iterate(black_tiles, &next_day/1)
    |> Enum.at(100)
    |> map_size()
    |> IO.puts()
  end

  defp parse_line(line) do
    line
    |> String.graphemes()
    |> parse_coords({0, 0})
  end

  defp parse_coords([], coord), do: coord
  defp parse_coords([h | t], {q, r}) do
    {dir, rest} = 
      case h do
        "e" -> {"e", t}
        "w" -> {"w", t}
        "n" -> parse_north(t)
        "s" -> parse_south(t)
      end
    
    {dq, dr} = @directions[dir]
    parse_coords(rest, {q + dq, r + dr})
  end

  defp parse_north(["e" | rest]), do: {"ne", rest}
  defp parse_north(["w" | rest]), do: {"nw", rest}

  defp parse_south(["e" | rest]), do: {"se", rest}
  defp parse_south(["w" | rest]), do: {"sw", rest}

  defp next_day(black_tiles) do
    tiles_to_check = 
      black_tiles
      |> Enum.flat_map(fn {tile, _} -> 
        [tile | get_neighbors(tile)]
      end)
      |> Enum.uniq()
      |> Enum.reduce(%{}, fn tile, acc -> Map.put(acc, tile, true) end)

    Enum.reduce(tiles_to_check, %{}, fn {tile, _}, new_black_tiles ->
      black_neighbor_count = 
        get_neighbors(tile)
        |> Enum.count(&Map.get(black_tiles, &1, false))

      cond do
        Map.get(black_tiles, tile, false) and (black_neighbor_count == 1 or black_neighbor_count == 2) ->
          Map.put(new_black_tiles, tile, true)
        not Map.get(black_tiles, tile, false) and black_neighbor_count == 2 ->
          Map.put(new_black_tiles, tile, true)
        true ->
          new_black_tiles
      end
    end)
  end

  defp get_neighbors({q, r}) do
    Enum.map(@directions, fn {_, {dq, dr}} -> {q + dq, r + dr} end)
  end
end

TileFlip.solve()
