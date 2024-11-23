
defmodule Solution do
  @open ?. 
  @trees ?|
  @lumberyard ?#
  @size 50

  def solve do
    grid = read_input("input.txt")
    
    {cycle_start, cycle_length, final_grid} = find_cycle(grid)
    
    remaining_minutes = rem(1_000_000_000 - cycle_start, cycle_length)
    final_grid = Enum.reduce(1..remaining_minutes, final_grid, fn _, acc -> transform(acc) end)
    
    {wooded, lumberyards} = count_resources(final_grid)
    IO.puts(wooded * lumberyards)
  end

  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  def find_cycle(initial_grid) do
    do_find_cycle(initial_grid, %{}, 0)
  end

  defp do_find_cycle(grid, seen_states, minute) do
    state = grid_to_string(grid)
    
    case Map.get(seen_states, state) do
      nil ->
        new_grid = transform(grid)
        do_find_cycle(new_grid, Map.put(seen_states, state, minute), minute + 1)
      
      seen_minute ->
        {seen_minute, minute - seen_minute, grid}
    end
  end

  def transform(grid) do
    for {row, i} <- Enum.with_index(grid) do
      for {_, j} <- Enum.with_index(row) do
        next_acre_state(grid, i, j)
      end
    end
  end

  defp next_acre_state(grid, i, j) do
    case Enum.at(Enum.at(grid, i), j) do
      @open ->
        if count_adjacent(grid, i, j, @trees) >= 3, do: @trees, else: @open
      
      @trees ->
        if count_adjacent(grid, i, j, @lumberyard) >= 3, do: @lumberyard, else: @trees
      
      @lumberyard ->
        if count_adjacent(grid, i, j, @lumberyard) >= 1 and 
           count_adjacent(grid, i, j, @trees) >= 1, 
           do: @lumberyard, 
           else: @open
    end
  end

  defp count_adjacent(grid, i, j, acre_type) do
    for x <- -1..1, y <- -1..1, 
        x != 0 or y != 0,
        i + x >= 0 and i + x < length(grid),
        j + y >= 0 and j + y < length(Enum.at(grid, 0)) do
      Enum.at(Enum.at(grid, i + x), j + y)
    end
    |> Enum.count(&(&1 == acre_type))
  end

  defp count_resources(grid) do
    wooded = 
      grid 
      |> List.flatten() 
      |> Enum.count(&(&1 == @trees))
    
    lumberyards = 
      grid 
      |> List.flatten() 
      |> Enum.count(&(&1 == @lumberyard))
    
    {wooded, lumberyards}
  end

  defp grid_to_string(grid) do
    grid
    |> Enum.map(&to_string/1)
    |> Enum.join("\n")
  end
end

Solution.solve()
