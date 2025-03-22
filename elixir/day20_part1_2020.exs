
defmodule Day20 do
  def rotate(tile) do
    tile
    |> Enum.zip()
    |> Enum.reverse()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&List.to_string/1)
  end

  def flip(tile) do
    Enum.map(tile, &String.reverse/1)
  end

  def get_borders(tile) do
    [
      Enum.at(tile, 0),
      Enum.at(tile, -1),
      tile |> Enum.map(&String.first/1) |> List.to_string(),
      tile |> Enum.map(&String.last/1) |> List.to_string()
    ]
  end

  def solve(input_file) do
    tiles =
      File.read!(input_file)
      |> String.split("\n", trim: true)
      |> Enum.chunk_by(fn line -> String.contains?(line, "Tile") end)
      |> Enum.chunk_every(2)
      |> Enum.map(fn
        [[tile_header], tile_data] ->
          tile_id =
            tile_header
            |> String.split([" ", ":"], trim: true)
            |> Enum.at(1)
            |> String.to_integer()

          {tile_id, tile_data}

        _ ->
          nil
      end)
      |> Enum.reject(&is_nil/1)
      |> Map.new()

    borders =
      tiles
      |> Enum.flat_map(fn {tile_id, tile} ->
        get_borders(tile)
        |> Enum.flat_map(fn border ->
          [
            {border, [tile_id]},
            {String.reverse(border), [tile_id]}
          ]
        end)
      end)
      |> Enum.group_by(fn {border, _} -> border end, fn {_, tile_id} -> tile_id end)
      |> Map.new(fn {border, tile_ids} -> {border, List.flatten(tile_ids)} end)

    corner_tiles =
      tiles
      |> Enum.filter(fn {tile_id, tile} ->
        matching_borders =
          get_borders(tile)
          |> Enum.count(fn border ->
            Map.get(borders, border, []) |> length() == 1
          end)

        matching_borders == 2
      end)
      |> Enum.map(fn {tile_id, _} -> tile_id end)

    Enum.reduce(corner_tiles, 1, fn tile_id, acc -> acc * tile_id end)
  end
end

IO.puts(Day20.solve("input.txt"))
