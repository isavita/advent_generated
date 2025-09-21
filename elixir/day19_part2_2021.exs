
defmodule Day19 do
  def main do
    input = File.read!("input.txt") |> String.trim()
    scanners = parse_input(input)
    ans = solve(scanners)
    IO.write("#{ans}\n")
  end

  # Parsing
  def parse_input(input) do
    blocks = String.split(input, "\n\n", trim: true)
    Enum.map(blocks, &parse_block/1)
  end

  def parse_block(block) do
    lines = String.split(block, "\n", trim: true)
    header = List.first(lines)

    [_, number_str] = Regex.run(~r/--- scanner (\d+) ---/, header)
    number = String.to_integer(number_str)

    coords =
      lines
      |> Enum.drop(1)
      |> Enum.map(fn line ->
        [xs, ys, zs] = String.split(line, ",")
        {String.to_integer(xs), String.to_integer(ys), String.to_integer(zs)}
      end)

    scanner = %{
      number: number,
      x: 0,
      y: 0,
      z: 0,
      relativeCoords: coords,
      absoluteCoords: [],
      absoluteCoordsMap: MapSet.new()
    }

    fill_rotations(scanner)
  end

  def fill_rotations(scanner) do
    coords = Map.get(scanner, :relativeCoords)

    dir2 = Enum.map(coords, fn {x, y, z} -> {x, -y, -z} end)
    dir3 = Enum.map(coords, fn {x, y, z} -> {x, -z, y} end)
    dir4 = Enum.map(coords, fn {x, y, z} -> {-y, -z, x} end)
    dir5 = Enum.map(coords, fn {x, y, z} -> {-x, -z, -y} end)
    dir6 = Enum.map(coords, fn {x, y, z} -> {y, -z, -x} end)

    six_rotations = [coords, dir2, dir3, dir4, dir5, dir6]

    final_rotations =
      Enum.flat_map(six_rotations, fn rotation ->
        r2 = Enum.map(rotation, fn {x, y, z} -> {-y, x, z} end)
        r3 = Enum.map(rotation, fn {x, y, z} -> {-x, -y, z} end)
        r4 = Enum.map(rotation, fn {x, y, z} -> {y, -x, z} end)
        [rotation, r2, r3, r4]
      end)

    Map.put(scanner, :rotations, final_rotations)
  end

  def fill_absolute_coords_map(scanner) do
    ac = Map.get(scanner, :absoluteCoords)
    if Enum.empty?(ac) do
      raise "absolute coords not set for scanner #{Map.get(scanner, :number)}"
    end

    map = MapSet.new(Enum.map(ac, fn {x, y, z} -> {x, y, z} end))
    Map.put(scanner, :absoluteCoordsMap, map)
  end

  def make_absolute_coords_list(abs, relative, relativeCoords) do
    {ax, ay, az} = abs
    {rx, ry, rz} = relative
    diff = {ax - rx, ay - ry, az - rz}

    Enum.map(relativeCoords, fn {cx, cy, cz} ->
      {elem(diff, 0) + cx, elem(diff, 1) + cy, elem(diff, 2) + cz}
    end)
  end

  # Core solving
  def solve(scanners) do
    first = List.first(scanners)
    first_with_abs = Map.put(first, :absoluteCoords, Map.get(first, :relativeCoords))
    first_with_abs = fill_absolute_coords_map(first_with_abs)

    settled = [first_with_abs]
    undetermined = Enum.drop(scanners, 1)

    loop(settled, undetermined)
  end

  def loop(settled, undetermined) do
    if Enum.empty?(undetermined) do
      furthest_distance(settled)
    else
      case align_one(settled, undetermined) do
        {:ok, {new_settled, new_undetermined}} -> loop(new_settled, new_undetermined)
        :error -> furthest_distance(settled)
      end
    end
  end

  def align_one(settled, undetermined) do
    Enum.with_index(undetermined)
    |> Enum.reduce_while({settled, undetermined}, fn {undet, idx}, {settled_acc, undetermined_acc} ->
      case align_once(undet, settled_acc) do
        {:ok, updated} ->
          new_settled = settled_acc ++ [updated]
          new_undetermined = List.delete_at(undetermined_acc, idx)
          {:halt, {:ok, {new_settled, new_undetermined}}}
        :error ->
          {:cont, {settled_acc, undetermined_acc}}
      end
    end)
  end

  def align_once(undet, settled) do
    rotations = Map.get(undet, :rotations, [])
    align_with_rotations(undet, settled, rotations)
  end

  def align_with_rotations(_undet, _settled, []) do
    :error
  end

  def align_with_rotations(undet, settled, [rotation | rest]) do
    case align_with_rotation(rotation, undet, settled) do
      {:ok, updated} -> {:ok, updated}
      :error -> align_with_rotations(undet, settled, rest)
    end
  end

  def align_with_rotation(rotation, undet, settled) do
    Enum.find_value(settled, nil, fn set ->
      Enum.find_value(set.absoluteCoords, nil, fn absCoord ->
        Enum.find_value(rotation, nil, fn relativeCoord ->
          offset = {
            elem(absCoord, 0) - elem(relativeCoord, 0),
            elem(absCoord, 1) - elem(relativeCoord, 1),
            elem(absCoord, 2) - elem(relativeCoord, 2)
          }

          abs_coords =
            Enum.map(rotation, fn {x, y, z} -> {elem(offset, 0) + x, elem(offset, 1) + y, elem(offset, 2) + z} end)

          matches = Enum.count(abs_coords, fn ac -> MapSet.member?(Map.get(set, :absoluteCoordsMap), ac) end)

          if matches >= 12 do
            updated =
              undet
              |> Map.put(:relativeCoords, rotation)
              |> Map.put(:absoluteCoords, abs_coords)
              |> fill_absolute_coords_map()
              |> Map.put(:x, elem(offset, 0))
              |> Map.put(:y, elem(offset, 1))
              |> Map.put(:z, elem(offset, 2))

            {:ok, updated}
          else
            nil
          end
        end)
      end)
    end)
    |> case do
      nil -> :error
      {:ok, updated} -> {:ok, updated}
    end
  end

  def furthest_distance(settled) do
    Enum.reduce(settled, 0, fn s1, acc ->
      Enum.reduce(settled, acc, fn s2, acc2 ->
        if s1 !== s2 do
          dist = abs(s1.x - s2.x) + abs(s1.y - s2.y) + abs(s1.z - s2.z)
          if dist > acc2, do: dist, else: acc2
        else
          acc2
        end
      end)
    end)
  end
end

Day19.main()
