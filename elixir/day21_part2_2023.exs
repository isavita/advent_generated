
defmodule Day21 do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  def find_start(grid) do
    grid
    |> Enum.with_index()
    |> Enum.reduce_while(nil, fn {row, r_idx}, acc ->
      case Enum.find_index(row, &(&1 == "S")) do
        nil ->
          {:cont, acc}

        c_idx ->
          {:halt, {r_idx, c_idx}}
      end
    end)
  end

  def neighbors({r, c}, grid) do
    height = length(grid)
    width = length(Enum.at(grid, 0))

    [
      {r - 1, c},
      {r + 1, c},
      {r, c - 1},
      {r, c + 1}
    ]
    |> Enum.filter(fn {nr, nc} ->
      nr >= 0 and nr < height and nc >= 0 and nc < width and Enum.at(Enum.at(grid, nr), nc) != "#"
    end)
  end

  def solve_part1(filename, steps) do
    grid = read_input(filename)
    start = find_start(grid)

    reachable =
      Enum.reduce(1..steps, MapSet.new([start]), fn _, acc ->
        acc
        |> MapSet.to_list()
        |> Enum.flat_map(&neighbors(&1, grid))
        |> MapSet.new()
      end)

    MapSet.size(reachable)
  end

  def neighbors_infinite({r, c}, grid) do
    height = length(grid)
    width = length(Enum.at(grid, 0))

    [
      {r - 1, c},
      {r + 1, c},
      {r, c - 1},
      {r, c + 1}
    ]
    |> Enum.filter(fn {nr, nc} ->
      grid_r = rem(nr, height)
      grid_c = rem(nc, width)

      Enum.at(Enum.at(grid, grid_r), grid_c) != "#"
    end)
  end

  def solve_part2(filename, steps) do
    grid = read_input(filename)
    start = find_start(grid)
    height = length(grid)
    width = length(Enum.at(grid, 0))

    # The input is crafted such that the start is in the middle and the rows and columns containing the start are clear.
    # This means we can reach the edges of the grid in a number of steps equal to half the grid size.
    # After that, the reachable plots grow quadratically.

    # We can therefore calculate the number of reachable plots for three different numbers of steps that allow us to reach the edges and beyond.
    n = div(steps, height)
    rem = rem(steps, height)

    reachable0 = reachable_plots(grid, start, rem)
    reachable1 = reachable_plots(grid, start, rem + height)
    reachable2 = reachable_plots(grid, start, rem + 2 * height)

    # Now, we can use the quadratic formula to extrapolate the number of reachable plots for the desired number of steps.
    a = div(reachable2 - 2 * reachable1 + reachable0, 2)
    b = reachable1 - reachable0 - a
    c = reachable0

    a * n * n + b * n + c
  end

  defp reachable_plots(grid, start, steps) do
    Enum.reduce(1..steps, MapSet.new([start]), fn _, acc ->
      acc
      |> MapSet.to_list()
      |> Enum.flat_map(&neighbors_infinite(&1, grid))
      |> MapSet.new()
    end)
    |> MapSet.size()
  end
end

# Main entry point
filename = "input.txt"
steps_part1 = 64
steps_part2 = 26501365

IO.puts("Part 1: #{Day21.solve_part1(filename, steps_part1)}")
IO.puts("Part 2: #{Day21.solve_part2(filename, steps_part2)}")
