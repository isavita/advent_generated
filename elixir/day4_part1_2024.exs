
defmodule WordSearch do
  def read_grid(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&String.to_charlist/1)
  end

  def all_directions do
    [
      {0, 1},
      {1, 0},
      {1, 1},
      {-1, 1},
      {0, -1},
      {-1, 0},
      {-1, -1},
      {1, -1}
    ]
  end

  def check_word(grid, word, x, y, {dx, dy}) do
    word_len = length(word)
    grid_height = length(grid)
    grid_width = length(List.first(grid))

    Enum.all?(0..word_len - 1, fn i ->
      new_x = x + dx * i
      new_y = y + dy * i
      new_x >= 0 and new_y >= 0 and new_x < grid_height and new_y < grid_width and
        Enum.at(grid, new_x) |> Enum.at(new_y) == Enum.at(word, i)
    end)
  end

  def count_occurrences(grid, word) do
    grid_height = length(grid)
    grid_width = length(List.first(grid))
    directions = all_directions()

    for x <- 0..grid_height - 1,
        y <- 0..grid_width - 1,
        dir <- directions,
        check_word(grid, String.to_charlist(word), x, y, dir),
        reduce: 0 do
      count -> count + 1
    end
  end

  def main(filename) do
    grid = read_grid(filename)
    count = count_occurrences(grid, "XMAS")
    IO.puts("XMAS appears #{count} times in the word search")
  end
end

WordSearch.main("input.txt")
