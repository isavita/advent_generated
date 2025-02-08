
defmodule Day14 do
  def knot_hash(input) do
    lengths =
      input
      |> String.to_charlist()
      |> Kernel.++([17, 31, 73, 47, 23])

    list = Enum.to_list(0..255)
    {dense_hash, _, _} =
      Enum.reduce(1..64, {list, 0, 0}, fn _, {list, current_position, skip_size} ->
        reduce_lengths(list, lengths, current_position, skip_size)
      end)
        dense_hash
        |> Enum.chunk_every(16)
        |> Enum.map(fn chunk -> Enum.reduce(chunk, 0, &Bitwise.bxor/2) end)
        |> Enum.map(&String.pad_leading(Integer.to_string(&1, 16), 2, "0"))
        |> Enum.join()
  end

  defp reduce_lengths(list, lengths, current_position, skip_size) do
    Enum.reduce(lengths, {list, current_position, skip_size}, fn length,
                                                                 {list, current_position,
                                                                  skip_size} ->
      reversed_section =
        for i <- 0..(length - 1), do: Enum.at(list, rem(current_position + i, 256))

      updated_list =
        Enum.reduce(0..(length - 1), list, fn i, acc ->
          List.replace_at(
            acc,
            rem(current_position + i, 256),
            Enum.at(reversed_section, length - 1 - i)
          )
        end)

      new_current_position = rem(current_position + length + skip_size, 256)
      new_skip_size = skip_size + 1
      {updated_list, new_current_position, new_skip_size}
    end)
  end

  def hex_to_binary(hex_string) do
    hex_string
    |> String.codepoints()
    |> Enum.map(fn hex_digit ->
      hex_digit
      |> String.to_integer(16)
      |> Integer.to_string(2)
      |> String.pad_leading(4, "0")
    end)
    |> Enum.join()
  end

  def solve_part1(key) do
    0..127
    |> Enum.map(fn row -> "#{key}-#{row}" end)
    |> Enum.map(&knot_hash/1)
    |> Enum.map(&hex_to_binary/1)
    |> Enum.map(&String.graphemes/1)
    |> List.flatten()
    |> Enum.count(fn bit -> bit == "1" end)
  end

  def solve_part2(key) do
    grid =
      0..127
      |> Enum.map(fn row -> "#{key}-#{row}" end)
      |> Enum.map(&knot_hash/1)
      |> Enum.map(&hex_to_binary/1)
      |> Enum.map(&String.graphemes/1)

    visited = MapSet.new()

    {regions, _} =
      Enum.reduce(0..127, {0, visited}, fn row, {region_count, visited} ->
        Enum.reduce(0..127, {region_count, visited}, fn col, {region_count, visited} ->
          if Enum.at(Enum.at(grid, row), col) == "1" and not MapSet.member?(visited, {row, col}) do
            visited = dfs(grid, row, col, visited)
            {region_count + 1, visited}
          else
            {region_count, visited}
          end
        end)
      end)

    regions
  end

  defp dfs(grid, row, col, visited) do
      if row < 0 || row >= 128 || col < 0 || col >= 128 || Enum.at(Enum.at(grid, row), col) == "0" || MapSet.member?(visited, {row, col}) do
        visited
      else
        visited = MapSet.put(visited, {row, col})
        visited = dfs(grid, row + 1, col, visited)
        visited = dfs(grid, row - 1, col, visited)
        visited = dfs(grid, row, col + 1, visited)
        visited = dfs(grid, row, col - 1, visited)
        visited
      end
    end
end

input =
  File.read!("input.txt")
  |> String.trim()

IO.puts("Part 1: #{Day14.solve_part1(input)}")
IO.puts("Part 2: #{Day14.solve_part2(input)}")
