defmodule PowerGrid do
  def call do
    serial_number = read_input()
    grid = build_grid(serial_number)
    {x, y, _} = find_max_power_square(grid)
    IO.puts("#{x},#{y}")
  end

  defp read_input do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.to_integer()
  end

  defp build_grid(serial_number) do
    for x <- 1..300, y <- 1..300, into: %{} do
      {{x, y}, power_level(x, y, serial_number)}
    end
  end

  defp power_level(x, y, serial_number) do
    rack_id = x + 10
    power_level = rack_id * y
    power_level = power_level + serial_number
    power_level = power_level * rack_id
    power_level = div(power_level, 100) |> rem(10)
    power_level - 5
  end

  defp find_max_power_square(grid) do
    1..298
    |> Enum.map(fn x ->
      1..298
      |> Enum.map(fn y ->
        square_power(grid, x, y)
      end)
      |> Enum.with_index()
      |> Enum.max_by(fn {power, _} -> power end)
      |> then(fn {power, index} -> {x, index + 1, power} end)
    end)
    |> Enum.max_by(fn {_, _, power} -> power end)
  end

  defp square_power(grid, x, y) do
    for i <- x..(x+2), j <- y..(y+2), reduce: 0 do
      acc -> acc + grid[{i, j}]
    end
  end
end

PowerGrid.call()
