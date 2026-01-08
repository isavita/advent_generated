
defmodule Main do
  @offsets for dy <- -1..1, dx <- -1..1, dy != 0 or dx != 0, do: {dy, dx}

  def main do
    lines =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    if lines == [] do
      IO.puts("Empty grid")
    else
      grid = Enum.map(lines, &String.to_charlist/1)
      rows = length(grid)
      cols = grid |> List.first() |> length()

      count =
        for y <- 0..(rows - 1), x <- 0..(cols - 1), grid |> Enum.at(y) |> Enum.at(x) == ?@ do
          neighbor_cnt =
            Enum.count(@offsets, fn {dy, dx} ->
              ny = y + dy
              nx = x + dx

              ny in 0..(rows - 1) and nx in 0..(cols - 1) and
                Enum.at(grid, ny) |> Enum.at(nx) == ?@
            end)

          if neighbor_cnt < 4, do: 1, else: 0
        end
        |> Enum.sum()

      IO.puts("Number of accessible rolls of paper: #{count}")
    end
  end
end

Main.main()
