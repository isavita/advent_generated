
defmodule DumboOctopus do
  def solve do
    grid = read_input("input.txt")
    
    {total_flashes, _} = 
      Enum.reduce(1..100, {0, grid}, fn _, {flashes, current_grid} ->
        {step_flashes, new_grid} = simulate_step(current_grid)
        {flashes + step_flashes, new_grid}
      end)
    
    IO.puts(total_flashes)
  end

  def read_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def simulate_step(grid) do
    # Increase energy by 1
    increased_grid = 
      Enum.map(grid, fn row -> 
        Enum.map(row, &(&1 + 1)) 
      end)
    
    # Perform flashes
    {flashes, final_grid} = 
      do_flashes(increased_grid, MapSet.new())
    
    {flashes, final_grid}
  end

  def do_flashes(grid, flashed) do
    # Find all positions that can flash
    flashable = 
      for {row, y} <- Enum.with_index(grid),
          {energy, x} <- Enum.with_index(row),
          energy > 9 and not MapSet.member?(flashed, {x, y}),
          do: {x, y}
    
    if Enum.empty?(flashable) do
      # Reset flashed positions to 0
      final_grid = 
        Enum.with_index(grid)
        |> Enum.map(fn {row, y} -> 
          Enum.with_index(row)
          |> Enum.map(fn {energy, x} -> 
            if MapSet.member?(flashed, {x, y}), do: 0, else: energy 
          end)
        end)
      
      {MapSet.size(flashed), final_grid}
    else
      # Perform flashes and propagate
      {new_grid, new_flashed} = 
        Enum.reduce(flashable, {grid, flashed}, fn {x, y}, {acc_grid, acc_flashed} ->
          flash(acc_grid, x, y, acc_flashed)
        end)
      
      do_flashes(new_grid, new_flashed)
    end
  end

  def flash(grid, x, y, flashed) do
    if MapSet.member?(flashed, {x, y}) do
      {grid, flashed}
    else
      new_flashed = MapSet.put(flashed, {x, y})
      
      # Directions for adjacent cells
      directions = [
        {-1, -1}, {-1, 0}, {-1, 1}, 
        {0, -1}, {0, 1}, 
        {1, -1}, {1, 0}, {1, 1}
      ]
      
      # Increase energy of adjacent cells
      new_grid = 
        Enum.reduce(directions, grid, fn {dx, dy}, acc_grid ->
          new_x = x + dx
          new_y = y + dy
          
          if new_x >= 0 and new_x < length(Enum.at(acc_grid, 0)) and 
             new_y >= 0 and new_y < length(acc_grid) do
            update_in(acc_grid, [Access.at(new_y), Access.at(new_x)], &(&1 + 1))
          else
            acc_grid
          end
        end)
      
      {new_grid, new_flashed}
    end
  end
end

DumboOctopus.solve()
