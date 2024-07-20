
defmodule ConwayCubes do
  @moduledoc """
  Simulates the Conway Cubes in a 3D pocket dimension.
  """

  def run do
    input = File.read!("input.txt")
    initial_state = parse_input(input)
    final_state = simulate(initial_state, 6)
    count_active(final_state)
  end

  defp parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      String.graphemes(line)
      |> Enum.with_index()
      |> Enum.map(fn {char, x} -> {{x, y, 0}, char == "#"} end)
    end)
    |> Map.new()
  end

  defp simulate(state, cycles) do
    Enum.reduce(1..cycles, state, fn _, acc -> next_state(acc) end)
  end

  defp next_state(state) do
    neighbors = for x <- -1..1, y <- -1..1, z <- -1..1, x != 0 or y != 0 or z != 0, do: {x, y, z}

    all_coords = Map.keys(state)
    new_coords = Enum.flat_map(all_coords, fn {x, y, z} ->
      for dx <- -1..1, dy <- -1..1, dz <- -1..1, do: {x + dx, y + dy, z + dz}
    end)
    |> Enum.uniq()

    Enum.reduce(new_coords, %{}, fn coord, acc ->
      active_neighbors = Enum.count(neighbors, fn {dx, dy, dz} ->
        Map.get(state, {elem(coord, 0) + dx, elem(coord, 1) + dy, elem(coord, 2) + dz}, false)
      end)

      new_state = case Map.get(state, coord, false) do
        true -> active_neighbors in [2, 3]
        false -> active_neighbors == 3
      end

      if new_state, do: Map.put(acc, coord, true), else: acc
    end)
  end

  defp count_active(state) do
    state
    |> Map.values()
    |> Enum.count(& &1)
  end
end

IO.puts ConwayCubes.run()
