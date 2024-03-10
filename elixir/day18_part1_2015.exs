defmodule LightAnimation do
  def call do
    input = read_input()

    # Parse the initial grid configuration
    grid = parse_grid(input)

    # Animate the grid for 100 steps
    final_grid = animate_grid(grid, 100)

    # Count the number of lights that are on
    lights_on = count_lights_on(final_grid)

    # Print the result
    IO.puts("After 100 steps, #{lights_on} lights are on.")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  defp parse_grid(input) do
    Enum.map(input, fn row ->
      String.graphemes(row)
      |> Enum.map(fn
        "#" -> true
        "." -> false
      end)
    end)
  end

  defp animate_grid(grid, steps) do
    Enum.reduce(1..steps, grid, fn _, acc -> animate_step(acc) end)
  end

  defp animate_step(grid) do
    rows = length(grid)
    cols = length(List.first(grid))

    Enum.map(0..(rows - 1), fn row ->
      Enum.map(0..(cols - 1), fn col ->
        neighbors_on = count_neighbors_on(grid, row, col)
        cell_state = Enum.at(Enum.at(grid, row), col)

        case {cell_state, neighbors_on} do
          {true, 2} -> true
          {true, 3} -> true
          {false, 3} -> true
          _ -> false
        end
      end)
    end)
  end

  defp count_neighbors_on(grid, row, col) do
    rows = length(grid)
    cols = length(List.first(grid))

    Enum.reduce((row - 1)..(row + 1), 0, fn i, acc ->
      Enum.reduce((col - 1)..(col + 1), acc, fn j, acc ->
        if i >= 0 and i < rows and j >= 0 and j < cols and {i, j} != {row, col} do
          if Enum.at(Enum.at(grid, i), j), do: acc + 1, else: acc
        else
          acc
        end
      end)
    end)
  end

  defp count_lights_on(grid) do
    Enum.sum(Enum.map(grid, fn row -> Enum.count(row, & &1) end))
  end
end

LightAnimation.call()
