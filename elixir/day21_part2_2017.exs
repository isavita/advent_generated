
defmodule ImageEnhancer do
  def main do
    rules = parse_rules("input.txt")
    initial_grid = [".#.", "..#", "###"]

    final_grid = run_enhancement(initial_grid, rules, 18)

    count = count_pixels(final_grid)
    IO.puts(count)
  end

  defp parse_rules(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [input, output] = String.split(line, " => ")
      {input, output}
    end)
    |> Map.new()
  end

  defp run_enhancement(grid, rules, iterations) do
    {final_grid, _final_memo} =
      Enum.reduce(1..iterations, {grid, %{}}, fn _i, {current_grid, memo} ->
        transform_grid(current_grid, rules, memo)
      end)
    final_grid
  end

  defp transform_grid(grid, rules, memo) do
    grid_size = length(grid)
    {sub_size, new_sub_size} = if rem(grid_size, 2) == 0, do: {2, 3}, else: {3, 4}
    num_blocks = div(grid_size, sub_size)

    {enhanced_patterns_by_coords, new_memo} =
      (for block_row <- 0..(num_blocks - 1),
           block_col <- 0..(num_blocks - 1) do
         {block_row, block_col}
       end)
      |> Enum.reduce({%{}, memo}, fn {block_row, block_col}, {acc_patterns, acc_memo} ->
           square_rows = Enum.slice(grid, block_row * sub_size, sub_size)
                         |> Enum.map(fn row ->
                            String.slice(row, block_col * sub_size, sub_size)
                         end)
           square_pattern = Enum.join(square_rows, "/")

           {enhanced_pattern, updated_memo} = find_match(square_pattern, rules, acc_memo)
           {Map.put(acc_patterns, {block_row, block_col}, enhanced_pattern), updated_memo}
         end)

    new_grid_rows =
      0..(num_blocks - 1)
      |> Enum.map(fn block_row ->
          enhanced_patterns_in_row =
            0..(num_blocks - 1)
            |> Enum.map(fn block_col ->
                 enhanced_patterns_by_coords[{block_row, block_col}]
               end)

          enhanced_rows_in_row =
            enhanced_patterns_in_row
            |> Enum.map(&String.split(&1, "/"))

          enhanced_rows_in_row
          |> List.zip()
          |> Enum.map(&Tuple.to_list/1)
          |> Enum.map(&Enum.join/1)
        end)
      |> List.flatten()

    {new_grid_rows, new_memo}
  end

  defp find_match(pattern, rules, memo) do
    if Map.has_key?(memo, pattern) do
      {Map.get(memo, pattern), memo}
    else
      match =
        orientations(pattern)
        |> Enum.find(nil, fn oriented_pattern -> Map.has_key?(rules, oriented_pattern) end)

      output = Map.get(rules, match)
      {output, Map.put(memo, pattern, output)}
    end
  end

  defp orientations(pattern) do
    rotated = rotate_all(pattern)
    flipped = flip(pattern)
    flipped_rotated = rotate_all(flipped)
    rotated ++ flipped_rotated |> Enum.uniq()
  end

  defp rotate_all(pattern) do
    r1 = rotate(pattern)
    r2 = rotate(r1)
    r3 = rotate(r2)
    [pattern, r1, r2, r3] |> Enum.uniq()
  end

  defp rotate(pattern) do
    parts = String.split(pattern, "/")
    size = length(parts)
    for x <- 0..(size - 1) do
      new_row_chars =
        for y <- (size - 1)..0, do: String.at(Enum.at(parts, y), x)
      Enum.join(new_row_chars)
    end
    |> Enum.join("/")
  end

  defp flip(pattern) do
    pattern
    |> String.split("/")
    |> Enum.map(&String.reverse/1)
    |> Enum.join("/")
  end

  defp count_pixels(grid) do
    grid
    |> Enum.map(&String.graphemes/1)
    |> List.flatten()
    |> Enum.count(&(&1 == "#"))
  end
end

ImageEnhancer.main()
