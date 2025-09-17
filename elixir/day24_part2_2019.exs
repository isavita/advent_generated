
defmodule RecursiveBugs do
  def main do
    initial = parse()
    space = %{0 => initial}
    space = Enum.reduce(1..200, space, fn _ , acc -> next_state(acc) end)
    total = Enum.reduce(space, 0, fn {_lvl, grid}, acc -> acc + Enum.count(grid, fn v -> v end) end)
    IO.puts(total)
  end

  def parse do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)
    lines = Enum.take(lines, 5)

    rows = for r <- 0..4 do
      line = Enum.at(lines, r, "")
      for c <- 0..4 do
        if String.length(line) > c and String.at(line, c) == "#" do true else false end
      end
    end

    List.flatten(rows)
  end

  def is_infested(space, level, cell) do
    case Map.get(space, level) do
      nil -> false
      grid -> Enum.at(grid, cell, false)
    end
  end

  def clean_space(space) do
    if map_size(space) == 0, do: space

    keys = Map.keys(space)
    min_l = Enum.min(keys)
    max_l = Enum.max(keys)

    min_grid = Map.get(space, min_l)
    min_empty = case min_grid do
      nil -> true
      grid -> Enum.all?(grid, fn v -> v == false end)
    end

    space = if min_empty, do: Map.delete(space, min_l), else: space
    if map_size(space) == 0, do: space

    keys2 = Map.keys(space)
    max_l2 = Enum.max(keys2)
    max_grid = Map.get(space, max_l2)
    max_empty = case max_grid do
      nil -> true
      grid -> Enum.all?(grid, fn v -> v == false end)
    end

    if max_empty, do: Map.delete(space, max_l2), else: space
  end

  def next_state(current_space) do
    keys = Map.keys(current_space)
    min_l = Enum.min(keys)
    max_l = Enum.max(keys)

    level_range = (min_l - 1)..(max_l + 1)

    new_space_list = Enum.map(level_range, fn level ->
      current_grid = Map.get(current_space, level, List.duplicate(false, 25))

      next_grid = Enum.map(0..24, fn cell ->
        if cell == 12 do
          Enum.at(current_grid, 12, false)
        else
          row = div(cell, 5)
          col = rem(cell, 5)
          neighbours = count_neighbours(current_space, level, cell, row, col)
          current_state = Enum.at(current_grid, cell, false)

          if current_state and neighbours != 1 do
            false
          else
            if (!current_state) and (neighbours == 1 or neighbours == 2) do
              true
            else
              current_state
            end
          end
        end
      end)

      {level, next_grid}
    end)

    new_space = Map.new(new_space_list)
    clean_space(new_space)
  end

  def count_neighbours(space, level, cell, row, col) do
    neighbours0 = 0
    n1 = if row == 0 and is_infested(space, level - 1, 7), do: neighbours0 + 1, else: neighbours0
    n2 = if col == 0 and is_infested(space, level - 1, 11), do: n1 + 1, else: n1
    n3 = if col == 4 and is_infested(space, level - 1, 13), do: n2 + 1, else: n2
    n4 = if row == 4 and is_infested(space, level - 1, 17), do: n3 + 1, else: n3

    n5 = if cell == 7 do
      Enum.reduce(0..4, n4, fn i, acc ->
        if is_infested(space, level + 1, i), do: acc + 1, else: acc
      end)
    else
      n4
    end

    n6 = if cell == 11 do
      Enum.reduce(0..4, n5, fn i, acc ->
        idx = 5 * i
        if is_infested(space, level + 1, idx), do: acc + 1, else: acc
      end)
    else
      n5
    end

    n7 = if cell == 13 do
      Enum.reduce(0..4, n6, fn i, acc ->
        idx = 5 * i + 4
        if is_infested(space, level + 1, idx), do: acc + 1, else: acc
      end)
    else
      n6
    end

    n8 = if cell == 17 do
      Enum.reduce(0..4, n7, fn i, acc ->
        idx = 20 + i
        if is_infested(space, level + 1, idx), do: acc + 1, else: acc
      end)
    else
      n7
    end

    n9 = if row > 0 and cell - 5 != 12 do
      if is_infested(space, level, cell - 5), do: n8 + 1, else: n8
    else
      n8
    end

    n10 = if col > 0 and cell - 1 != 12 do
      if is_infested(space, level, cell - 1), do: n9 + 1, else: n9
    else
      n9
    end

    n11 = if col < 4 and cell + 1 != 12 do
      if is_infested(space, level, cell + 1), do: n10 + 1, else: n10
    else
      n10
    end

    if row < 4 and cell + 5 != 12 do
      if is_infested(space, level, cell + 5), do: n11 + 1, else: n11
    else
      n11
    end
  end
end

RecursiveBugs.main()
