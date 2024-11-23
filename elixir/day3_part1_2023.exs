
defmodule PartNumbers do
  def solve do
    {:ok, content} = File.read("input.txt")
    matrix = String.split(content, "\n", trim: true) |> Enum.map(&String.graphemes/1)
    
    sum_part_numbers(matrix)
    |> IO.puts()
  end

  def sum_part_numbers(matrix) do
    matrix
    |> Enum.with_index()
    |> Enum.reduce(0, fn {row, y}, acc ->
      row
      |> Enum.with_index()
      |> Enum.reduce({acc, MapSet.new()}, fn {_, x}, {sum, visited} ->
        if MapSet.member?(visited, {y, x}) or not is_digit?(Enum.at(row, x)) do
          {sum, visited}
        else
          {number, length, new_visited} = extract_number(matrix, x, y, visited)
          
          if is_adjacent_to_symbol?(matrix, x, y, length) do
            {sum + number, new_visited}
          else
            {sum, new_visited}
          end
        end
      end)
      |> elem(0)
    end)
  end

  defp extract_number(matrix, x, y, visited) do
    row = Enum.at(matrix, y)
    
    {number_str, length, new_visited} = 
      Enum.reduce_while(x..(length(row)-1), {"", 0, visited}, fn curr_x, {num_str, len, curr_visited} ->
        curr_char = Enum.at(row, curr_x)
        
        if is_digit?(curr_char) do
          {:cont, {num_str <> curr_char, len + 1, MapSet.put(curr_visited, {y, curr_x})}}
        else
          {:halt, {num_str, len, curr_visited}}
        end
      end)
    
    {String.to_integer(number_str), length, new_visited}
  end

  defp is_adjacent_to_symbol?(matrix, x, y, length) do
    Enum.any?(0..(length-1), fn offset ->
      check_adjacent(matrix, x + offset, y)
    end)
  end

  defp check_adjacent(matrix, x, y) do
    for dy <- -1..1, dx <- -1..1, reduce: false do
      acc ->
        adj_x = x + dx
        adj_y = y + dy
        
        cond do
          acc -> true
          adj_y < 0 or adj_y >= length(matrix) -> false
          adj_x < 0 or adj_x >= length(Enum.at(matrix, adj_y)) -> false
          true ->
            adj_char = matrix |> Enum.at(adj_y) |> Enum.at(adj_x)
            not is_digit?(adj_char) and adj_char != "."
        end
    end
  end

  defp is_digit?(char) do
    String.match?(char, ~r/\d/)
  end
end

PartNumbers.solve()
