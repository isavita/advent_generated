
defmodule Day11 do
  def solve do
    grid = read_input("input.txt")
    {_, step} = solve_steps(grid, 0)
    step
  end

  def read_input(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {row_str, y} ->
      String.graphemes(row_str)
      |> Enum.with_index()
      |> Enum.map(fn {digit_str, x} ->
        {{x, y}, String.to_integer(digit_str)}
      end)
    end)
    |> Map.new()
  end

  def solve_steps(grid, step) do
    {next_grid, flashes} = simulate_step(grid)
    if flashes == 100 do
      {next_grid, step + 1}
    else
      solve_steps(next_grid, step + 1)
    end
  end

  def simulate_step(grid) do
    grid =
      grid
      |> Map.to_list()
      |> Enum.map(fn {{x, y}, energy} -> {{x, y}, energy + 1} end)
      |> Map.new()

    {grid, _} = flash_step(grid, MapSet.new())
    flashed_count =
      grid
      |> Map.values()
      |> Enum.count(fn energy -> energy == 0 end)
    {grid, flashed_count}
  end

  def flash_step(grid, flashed) do
    flashing =
      grid
      |> Enum.filter(fn {coords, energy} -> energy > 9 and not MapSet.member?(flashed, coords) end)
      |> Enum.map(fn {{x, y}, _} -> {x, y} end)

    if Enum.empty?(flashing) do
      grid =
        grid
        |> Map.to_list()
        |> Enum.map(fn {coords, energy} ->
          if energy > 9 do
            {coords, 0}
          else
            {coords, energy}
          end
        end)
        |> Map.new()
      {grid, flashed}
    else
      {next_grid, next_flashed} =
        flashing
        |> Enum.reduce({grid, flashed}, fn {flash_x, flash_y}, {current_grid, current_flashed} ->
          {updated_grid, updated_flashed} = apply_flash(current_grid, current_flashed, flash_x, flash_y)
          {updated_grid, MapSet.union(current_flashed, updated_flashed)}
        end)
      flash_step(next_grid, next_flashed)
    end
  end

  def apply_flash(grid, flashed, x, y) do
    if MapSet.member?(flashed, {x, y}) do
      {grid, flashed}
    else
      flashed = MapSet.put(flashed, {x, y})
      neighbors = get_neighbors(x, y)

      grid =
        neighbors
        |> Enum.reduce(grid, fn {nx, ny}, current_grid ->
          if Map.has_key?(current_grid, {nx, ny}) do
            Map.update!(current_grid, {nx, ny}, &(&1 + 1))
          else
            current_grid
          end
        end)
      {grid, flashed}
    end
  end

  def get_neighbors(x, y) do
    for dx <- -1..1, dy <- -1..1, dx != 0 or dy != 0 do
      {x + dx, y + dy}
    end
  end
end

IO.puts(Day11.solve())
