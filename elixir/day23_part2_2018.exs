defmodule Nanobots do
  def main() do
    nanobots = parse_input("input.txt")
    ans1 = part_one(nanobots)
    IO.puts("Part One: #{ans1}")
    ans2 = part_two(nanobots)
    IO.puts("Part Two: #{ans2}")
  end

  def parse_input(file_path) do
    pattern = ~r/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/
    with {:ok, content} <- File.read(file_path) do
      content
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        case Regex.run(pattern, String.trim(line)) do
          [_, xs, ys, zs, rs] -> {String.to_integer(xs), String.to_integer(ys), String.to_integer(zs), String.to_integer(rs)}
          _ -> nil
        end
      end)
      |> Enum.filter(& &1)
    else
      _ -> []
    end
  end

  def part_one(nanobots) do
    {sx, sy, sz, sr} = Enum.max_by(nanobots, fn {_, _, _, r} -> r end)
    Enum.count(nanobots, fn {bx, by, bz, _} -> manhattan({sx, sy, sz}, {bx, by, bz}) <= sr end)
  end

  def manhattan(a, b) do
    abs(elem(a, 0) - elem(b, 0)) +
      abs(elem(a, 1) - elem(b, 1)) +
      abs(elem(a, 2) - elem(b, 2))
  end

  def min_distance_to_origin(x, y, z, size) do
    dx =
      if x > 0 do
        x
      else
        if x + size - 1 < 0, do: -(x + size - 1), else: 0
      end
    dy =
      if y > 0 do
        y
      else
        if y + size - 1 < 0, do: -(y + size - 1), else: 0
      end
    dz =
      if z > 0 do
        z
      else
        if z + size - 1 < 0, do: -(z + size - 1), else: 0
      end
    dx + dy + dz
  end

  def part_two(nanobots) do
    min_x = nanobots |> Enum.map(fn {x, _, _, _} -> x end) |> Enum.min()
    max_x = nanobots |> Enum.map(fn {x, _, _, _} -> x end) |> Enum.max()
    min_y = nanobots |> Enum.map(fn {_, y, _, _} -> y end) |> Enum.min()
    max_y = nanobots |> Enum.map(fn {_, y, _, _} -> y end) |> Enum.max()
    min_z = nanobots |> Enum.map(fn {_, _, z, _} -> z end) |> Enum.min()
    max_z = nanobots |> Enum.map(fn {_, _, z, _} -> z end) |> Enum.max()

    max_diff = max(max_x - min_x, max(max_y - min_y, max_z - min_z))
    size = compute_initial_size(max_diff)
    distance0 = min_distance_to_origin(min_x, min_y, min_z, size)
    heap = [{0, distance0, size, min_x, min_y, min_z}]
    loop(nanobots, heap, -1, nil)
  end

  def compute_initial_size(max_diff) do
    do_compute_initial_size(1, max_diff)
  end

  def do_compute_initial_size(size, max_diff) when size < max_diff, do: do_compute_initial_size(size * 2, max_diff)
  def do_compute_initial_size(size, _max_diff), do: size

  def count_in_cube(nanobots, nx, ny, nz, size) do
    Enum.count(nanobots, fn {bx, by, bz, br} ->
      d =
        (if bx < nx, do: nx - bx, else: 0) +
        (if bx > nx + size - 1, do: bx - (nx + size - 1), else: 0) +
        (if by < ny, do: ny - by, else: 0) +
        (if by > ny + size - 1, do: by - (ny + size - 1), else: 0) +
        (if bz < nz, do: nz - bz, else: 0) +
        (if bz > nz + size - 1, do: bz - (nz + size - 1), else: 0)
      d <= br
    end)
  end

  def loop(nanobots, heap, best_count, best_distance) do
    case heap do
      [] -> best_distance
      [head | tail] ->
        {neg_count, distance, size, x, y, z} = head
        count = -neg_count

        if size == 1 do
          cond do
            count > best_count -> distance
            count == best_count and (best_distance == nil or distance < best_distance) -> distance
            true -> loop(nanobots, tail, best_count, best_distance)
          end
        else
          half = div(size, 2)
          new_nodes =
            for dx <- [0, half], dy <- [0, half], dz <- [0, half] do
              nx = x + dx
              ny = y + dy
              nz = z + dz
              new_size = half
              new_size = if new_size < 1, do: 1, else: new_size
              c = count_in_cube(nanobots, nx, ny, nz, new_size)
              dist2 = min_distance_to_origin(nx, ny, nz, new_size)
              {-c, dist2, new_size, nx, ny, nz}
            end

          heap2 = (tail ++ new_nodes) |> Enum.sort_by(fn {neg, dist, _s, _x, _y, _z} -> {neg, dist} end)
          loop(nanobots, heap2, best_count, best_distance)
        end
    end
  end
end

Nanobots.main()