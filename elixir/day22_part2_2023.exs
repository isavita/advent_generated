
defmodule Day22 do
  def read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_brick/1)
    |> Enum.sort_by(&(&1.z1))
  end

  defp parse_brick(line) do
    [start, finish] = String.split(line, "~")
    [x1, y1, z1] = String.split(start, ",") |> Enum.map(&String.to_integer/1)
    [x2, y2, z2] = String.split(finish, ",") |> Enum.map(&String.to_integer/1)
    %{x1: x1, y1: y1, z1: z1, x2: x2, y2: y2, z2: z2}
  end

  def settle(bricks) do
    settled_bricks =
      Enum.reduce(bricks, {%{}, []}, fn brick, {occupied, settled} ->
        settle_brick(brick, occupied, settled)
      end)
      |> elem(1)

    supports = build_supports(settled_bricks)
    supported_by = build_supported_by(supports)

    {settled_bricks, supports, supported_by}
  end

  defp settle_brick(brick, occupied, settled) do
    max_z =
      occupied_positions(brick)
      |> Enum.map(fn {x, y} ->
        Map.get(occupied, {x, y}, 0)
      end)
      |> Enum.max()

    dz = brick.z1 - max_z - 1
    new_brick = %{brick | z1: brick.z1 - dz, z2: brick.z2 - dz}

    new_occupied =
      occupied_positions(new_brick)
      |> Enum.reduce(occupied, fn {x, y}, acc ->
        Map.put(acc, {x, y}, new_brick.z2)
      end)

    {new_occupied, [new_brick | settled]}
  end

  defp occupied_positions(brick) do
    for x <- brick.x1..brick.x2,
        y <- brick.y1..brick.y2,
        do: {x, y}
  end

  def build_supports(bricks) do
    bricks
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {brick, i}, acc ->
      supported =
        bricks
        |> Enum.with_index()
        |> Enum.filter(fn {other_brick, j} ->
          i != j && brick_supports?(brick, other_brick)
        end)
        |> Enum.map(fn {_, j} -> j end)

      Map.put(acc, i, supported)
    end)
  end

  defp brick_supports?(brick1, brick2) do
    brick1.z2 + 1 == brick2.z1 && overlap?(brick1, brick2)
  end

  defp overlap?(brick1, brick2) do
    max(brick1.x1, brick2.x1) <= min(brick1.x2, brick2.x2) and
      max(brick1.y1, brick2.y1) <= min(brick1.y2, brick2.y2)
  end

  def build_supported_by(supports) do
    all_indices = Map.keys(supports)

    all_indices
    |> Enum.reduce(%{}, fn i, acc ->
      supported_by =
        supports
        |> Enum.filter(fn {_, supported} -> i in supported end)
        |> Enum.map(fn {j, _} -> j end)

      Map.put(acc, i, supported_by)
    end)
  end

  def part1(filename) do
    bricks = read_input(filename)
    {_, supports, supported_by} = settle(bricks)

    bricks
    |> Enum.with_index()
    |> Enum.count(fn {_, i} ->
      supports[i]
      |> Enum.all?(fn j -> length(supported_by[j]) > 1 end)
    end)
  end

  def part2(filename) do
    bricks = read_input(filename)
    {settled_bricks, supports, supported_by} = settle(bricks)

    bricks
    |> Enum.with_index()
    |> Enum.map(fn {_, i} ->
      fallen = count_falling(i, supports, supported_by, MapSet.new([i]))
      Enum.count(fallen) - 1  # Subtract the disintegrated brick itself.
    end)
    |> Enum.sum()
  end
  
  def count_falling(i, supports, supported_by, fallen_set) do
        supports[i]
        |> Enum.reduce(fallen_set, fn j, acc ->
            if Enum.all?(supported_by[j], &(&1 in acc)) do
               count_falling(j, supports, supported_by, MapSet.put(acc, j))
            else
                acc
            end
        end)
  end
end

input_file = "input.txt"
IO.puts("Part 1: #{Day22.part1(input_file)}")
IO.puts("Part 2: #{Day22.part2(input_file)}")
