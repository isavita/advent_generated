
defmodule Day19 do
  def solve do
    File.read!("input.txt")
    |> String.trim()
    |> parse_input()
    |> solve_scanners()
    |> MapSet.size()
  end

  defp solve_scanners(scanners) do
    {settled, undetermined} =
      List.pop_at(scanners, 0)
      |> then(fn scanner ->
        scanner = %{scanner | absolute_coords: scanner.relative_coords, absolute_coords_map: MapSet.new(scanner.relative_coords)}
        {[scanner], scanners}
      end)

    Enum.reduce_while(undetermined, settled, fn _, settled ->
      case find_absolute_coords_for_scanner(settled, undetermined) do
        {:ok, updated_scanner, remaining_undetermined} ->
          {:cont, [updated_scanner | settled] , remaining_undetermined}
        :error ->
          {:halt, settled}
      end
    end)
    |> Enum.flat_map(& &1.absolute_coords_map)
    |> MapSet.new()
  end

  defp find_absolute_coords_for_scanner(settled, undetermined) do
    Enum.find_value(undetermined, :error, fn undet ->
      Enum.find_value(undet.rotations, :error, fn rotated_coords ->
        Enum.find_value(settled, :error, fn set ->
          Enum.find_value(set.absolute_coords, :error, fn abs_coord ->
            Enum.find_value(rotated_coords, :error, fn relative_coord ->
              unsettled_absolute_coords = make_absolute_coords_list(abs_coord, relative_coord, rotated_coords)
              matching_count = Enum.count(unsettled_absolute_coords, &MapSet.member?(set.absolute_coords_map, &1))

              if matching_count >= 12 do
                updated_scanner = %{
                  undet
                  | relative_coords: rotated_coords,
                    absolute_coords: unsettled_absolute_coords,
                    absolute_coords_map: MapSet.new(unsettled_absolute_coords),
                    x: abs_coord[0] - relative_coord[0],
                    y: abs_coord[1] - relative_coord[1],
                    z: abs_coord[2] - relative_coord[2]
                }
                {:ok, updated_scanner, List.delete(undetermined, undet)}
              else
                :error
              end
            end)
          end)
        end)
      end)
    end)
  end

  defp make_absolute_coords_list(absolute, relative, relative_coords) do
    diff = {absolute[0] - relative[0], absolute[1] - relative[1], absolute[2] - relative[2]}
    Enum.map(relative_coords, fn c -> {diff[0] + c[0], diff[1] + c[1], diff[2] + c[2]} end)
  end

  defp parse_input(input) do
    input
    |> String.split("\n\n")
    |> Enum.map(fn raw_scanner ->
      [header | lines] = String.split(raw_scanner, "\n")
      {_, number} = Regex.run(~r/--- scanner (\d+) ---/, header) |> List.to_tuple()
      coords = Enum.map(lines, &parse_coords/1)
      %{
        number: String.to_integer(number),
        x: 0,
        y: 0,
        z: 0,
        relative_coords: coords,
        absolute_coords: [],
        absolute_coords_map: MapSet.new(),
        rotations: fill_rotations(coords)
      }
    end)
  end

  defp parse_coords(line) do
    [x, y, z] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
    {x, y, z}
  end

  defp fill_rotations(coords) do
    pos_x = coords
    dir2 = Enum.map(pos_x, fn {x, y, z} -> {x, -y, -z} end)
    dir3 = Enum.map(pos_x, fn {x, y, z} -> {x, -z, y} end)
    dir4 = Enum.map(pos_x, fn {x, y, z} -> {-y, -z, x} end)
    dir5 = Enum.map(pos_x, fn {x, y, z} -> {-x, -z, -y} end)
    dir6 = Enum.map(pos_x, fn {x, y, z} -> {y, -z, -x} end)

    six_rotations = [pos_x, dir2, dir3, dir4, dir5, dir6]

    for rotation <- six_rotations,
        r2 = Enum.map(rotation, fn {x, y, z} -> {-y, x, z} end),
        r3 = Enum.map(rotation, fn {x, y, z} -> {-x, -y, z} end),
        r4 = Enum.map(rotation, fn {x, y, z} -> {y, -x, z} end)
    do
      [rotation, r2, r3, r4]
    end
    |> List.flatten()
  end
end

Day19.solve()
