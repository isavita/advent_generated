
defmodule Solution do
  def check_mas(grid, x, y, dx, dy, word) do
    len = String.length(word)

    check_dir = fn dir ->
      Enum.all?(0..(len - 1), fn i ->
        new_x = x + dx * i
        new_y = y + dy * i

        if new_x < 0 || new_y < 0 || new_x >= length(grid) || new_y >= String.length(Enum.at(grid, new_x)) do
          false
        else
          String.at(Enum.at(grid, new_x), new_y) ==
            String.at(word, if(dir == :forward, do: i, else: len - 1 - i))
        end
      end)
    end

    check_dir.(:forward) || check_dir.(:backward)
  end

  def check_xmas(grid, x, y) do
    (check_mas(grid, x - 1, y - 1, 1, 1, "MAS") &&
       check_mas(grid, x - 1, y + 1, 1, -1, "MAS")) ||
      (check_mas(grid, x + 1, y - 1, -1, 1, "MAS") &&
         check_mas(grid, x + 1, y + 1, -1, -1, "MAS"))
  end

  def count_xmas_patterns(grid) do
    rows = length(grid)
    cols = if rows > 0, do: String.length(Enum.at(grid, 0)), else: 0

    if rows < 3 || cols < 3 do
      0
    else
      for i <- 1..(rows - 2), j <- 1..(cols - 2), reduce: 0 do
        acc ->
          if String.at(Enum.at(grid, i), j) == "A" && check_xmas(grid, i, j) do
            acc + 1
          else
            acc
          end
      end
    end
  end

  def main() do
    grid =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    count = count_xmas_patterns(grid)
    IO.puts("X-MAS patterns appear #{count} times in the word search")
  end
end

Solution.main()
