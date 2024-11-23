
defmodule Solution do
  def solve do
    {:ok, input} = File.read("input.txt")
    [x_range, y_range] = String.split(input, ", ")
    
    [x_min, x_max] = 
      x_range 
      |> String.slice(15..-1) 
      |> String.split("..")
      |> Enum.map(&String.to_integer/1)
    
    [y_min, y_max] = 
      y_range 
      |> String.slice(2..-1) 
      |> String.split("..")
      |> Enum.map(&String.to_integer/1)
    
    result = 
      for xVel <- -1000..1000,
          yVel <- -1000..1000,
          reduce: -1 do
        max_y -> 
          case simulate(xVel, yVel, x_min, x_max, y_min, y_max) do
            {:hit, height} -> max(max_y, height)
            :miss -> max_y
          end
      end
    
    IO.puts(result)
  end
  
  defp simulate(x_vel, y_vel, x_min, x_max, y_min, y_max) do
    do_simulate(0, 0, x_vel, y_vel, x_min, x_max, y_min, y_max, 0)
  end
  
  defp do_simulate(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max, highest_y) 
       when x_pos >= x_min and x_pos <= x_max and y_pos >= y_min and y_pos <= y_max do
    {:hit, highest_y}
  end
  
  defp do_simulate(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max, highest_y) 
       when x_pos < x_min and x_vel < 0 or 
            x_pos > x_max and x_vel > 0 or 
            y_pos < y_min and y_vel < 0 do
    :miss
  end
  
  defp do_simulate(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max, highest_y) do
    new_x_pos = x_pos + x_vel
    new_y_pos = y_pos + y_vel
    
    new_x_vel = 
      cond do
        x_vel > 0 -> x_vel - 1
        x_vel < 0 -> x_vel + 1
        true -> 0
      end
    
    new_y_vel = y_vel - 1
    new_highest_y = max(highest_y, new_y_pos)
    
    do_simulate(
      new_x_pos, 
      new_y_pos, 
      new_x_vel, 
      new_y_vel, 
      x_min, 
      x_max, 
      y_min, 
      y_max, 
      new_highest_y
    )
  end
end

Solution.solve()
