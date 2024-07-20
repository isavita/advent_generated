
defmodule BugSimulation do
  @grid_size 5

  def run do
    input = File.read!("input.txt")
    initial_state = parse_input(input)
    find_repeated_layout(initial_state, MapSet.new())
  end

  defp parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.graphemes/1)
  end

  defp find_repeated_layout(state, seen_states) do
    current_state = state |> to_string() |> String.trim()
    
    if MapSet.member?(seen_states, current_state) do
      calculate_biodiversity(state)
    else
      next_state = evolve(state)
      find_repeated_layout(next_state, MapSet.put(seen_states, current_state))
    end
  end

  defp evolve(state) do
    for row <- 0..(@grid_size - 1) do
      for col <- 0..(@grid_size - 1) do
        current_tile = Enum.at(Enum.at(state, row), col)
        adjacent_bugs = count_adjacent_bugs(state, row, col)

        case {current_tile, adjacent_bugs} do
          {"#", 1} -> "#"
          {"#", _} -> "."
          {".", 1} -> "#"
          {".", 2} -> "#"
          {".", _} -> "."
        end
      end
    end
  end

  defp count_adjacent_bugs(state, row, col) do
    directions = [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
    
    Enum.reduce(directions, 0, fn {dx, dy}, acc ->
      new_row = row + dx
      new_col = col + dy

      if new_row in 0..(@grid_size - 1) and new_col in 0..(@grid_size - 1) do
        if Enum.at(Enum.at(state, new_row), new_col) == "#" do
          acc + 1
        else
          acc
        end
      else
        acc
      end
    end)
  end

  defp calculate_biodiversity(state) do
    state
    |> Enum.with_index()
    |> Enum.reduce(0, fn {row, row_index}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {cell, col_index}, acc2 ->
        if cell == "#" do
          acc2 + :math.pow(2, row_index * @grid_size + col_index) |> round()
        else
          acc2
        end
      end)
    end)
  end
end

BugSimulation.run() |> IO.puts()
