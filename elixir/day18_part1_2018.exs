
defmodule LumberCollection do
  @input_file "input.txt"
  @width 50
  @height 50

  def run do
    initial_state = read_input()
    final_state = simulate(initial_state, 10)
    resource_value(final_state)
  end

  defp read_input do
    File.read!(@input_file)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  defp simulate(state, 0), do: state

  defp simulate(state, minutes) do
    new_state = Enum.map(0..(@height - 1), fn y ->
      Enum.map(0..(@width - 1), fn x ->
        update_acre(state, x, y)
      end)
    end)
    simulate(new_state, minutes - 1)
  end

  defp update_acre(state, x, y) do
    acre = Enum.at(Enum.at(state, y), x)
    adjacent = adjacent_acres(state, x, y)

    case acre do
      ?\. -> if count_trees(adjacent) >= 3, do: ?|, else: ?.
      ?| -> if count_lumberyards(adjacent) >= 3, do: ?#, else: ?|
      ?# -> if count_trees(adjacent) >= 1 and count_lumberyards(adjacent) >= 1, do: ?#, else: ?.
    end
  end

  defp adjacent_acres(state, x, y) do
    for dy <- -1..1, dx <- -1..1, 
        not (dy == 0 and dx == 0),
        into: [] do
      nx = x + dx
      ny = y + dy
      if nx >= 0 and nx < @width and ny >= 0 and ny < @height do
        Enum.at(Enum.at(state, ny), nx)
      end
    end
  end

  defp count_trees(acres) do
    Enum.count(acres, &(&1 == ?|))
  end

  defp count_lumberyards(acres) do
    Enum.count(acres, &(&1 == ?#))
  end

  defp resource_value(state) do
    trees = Enum.flat_map(state, & &1) |> Enum.count(&(&1 == ?|))
    lumberyards = Enum.flat_map(state, & &1) |> Enum.count(&(&1 == ?#))
    trees * lumberyards
  end
end

LumberCollection.run() |> IO.puts()
