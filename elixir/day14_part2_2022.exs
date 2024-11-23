
defmodule Solution do
  def solve(input) do
    matrix = parse_input(input)
    origin_col = Enum.find_index(Enum.at(matrix, 0), fn x -> x == "+" end)
    
    matrix = List.update_at(matrix, -1, fn row -> 
      Enum.map(row, fn _ -> "#" end)
    end)

    do_drop_sand(matrix, origin_col, 0)
  end

  defp do_drop_sand(matrix, origin_col, count) do
    cond do
      Enum.at(matrix, 0) |> Enum.at(origin_col) == "o" -> 
        count
      true ->
        case drop_sand(matrix, origin_col) do
          {:ok, new_matrix} -> do_drop_sand(new_matrix, origin_col, count + 1)
          :abyss -> count
        end
    end
  end

  defp drop_sand(matrix, origin_col) do
    do_drop_sand_step(matrix, 0, origin_col)
  end

  defp do_drop_sand_step(matrix, row, col) when row + 1 >= length(matrix) do
    :abyss
  end

  defp do_drop_sand_step(matrix, row, col) do
    below = matrix |> Enum.at(row + 1) |> Enum.at(col)
    left = matrix |> Enum.at(row + 1) |> Enum.at(col - 1)
    right = matrix |> Enum.at(row + 1) |> Enum.at(col + 1)

    cond do
      below == "." -> do_drop_sand_step(matrix, row + 1, col)
      left == "." -> do_drop_sand_step(matrix, row + 1, col - 1)
      right == "." -> do_drop_sand_step(matrix, row + 1, col + 1)
      true -> 
        new_matrix = matrix 
        |> List.update_at(row, fn r -> List.replace_at(r, col, "o") end)
        {:ok, new_matrix}
    end
  end

  def parse_input(input) do
    coord_sets = input 
    |> String.split("\n")
    |> Enum.map(fn line -> 
      line 
      |> String.split(" -> ")
      |> Enum.map(fn coord -> 
        coord 
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple
      end)
    end)

    {lowest_col, highest_row} = find_bounds(coord_sets)
    extra_left_space = 200

    coord_sets = Enum.map(coord_sets, fn set -> 
      Enum.map(set, fn {col, row} -> 
        {col - lowest_col + extra_left_space, row}
      end)
    end)

    highest_col = find_highest_col(coord_sets)

    matrix = List.duplicate(List.duplicate(".", highest_col + extra_left_space * 2 + 1), highest_row + 3)
    
    matrix = Enum.reduce(coord_sets, matrix, fn set, acc ->
      draw_path(acc, set)
    end)

    origin_col = 500 - lowest_col + extra_left_space
    matrix 
    |> List.update_at(0, fn row -> List.replace_at(row, origin_col, "+") end)
  end

  defp find_bounds(coord_sets) do
    Enum.reduce(coord_sets, {999999, 0}, fn set, {lowest, highest} ->
      {
        Enum.min([lowest | Enum.map(set, fn {col, _} -> col end)]),
        Enum.max([highest | Enum.map(set, fn {_, row} -> row end)])
      }
    end)
  end

  defp find_highest_col(coord_sets) do
    Enum.max(Enum.flat_map(coord_sets, fn set -> 
      Enum.map(set, fn {col, _} -> col end)
    end))
  end

  defp draw_path(matrix, [_]), do: matrix
  defp draw_path(matrix, [{col1, row1}, {col2, row2} | rest]) do
    new_matrix = cond do
      col1 == col2 -> 
        Enum.reduce(min(row1, row2)..max(row1, row2), matrix, fn r, acc -> 
          List.update_at(acc, r, fn row -> List.replace_at(row, col1, "#") end)
        end)
      row1 == row2 -> 
        Enum.reduce(min(col1, col2)..max(col1, col2), matrix, fn c, acc -> 
          List.update_at(acc, row1, fn row -> List.replace_at(row, c, "#") end)
        end)
      true -> matrix
    end
    draw_path(new_matrix, [{col2, row2} | rest])
  end

  def main do
    {:ok, input} = File.read("input.txt")
    input = String.trim(input)
    IO.puts(solve(input))
  end
end

Solution.main()
