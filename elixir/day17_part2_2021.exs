
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
    
    velocities = 
      for x_vel <- -1000..1000,
          y_vel <- -1000..1000,
          hit_target?(x_vel, y_vel, x_min, x_max, y_min, y_max),
          do: {x_vel, y_vel}
    
    length(velocities)
    |> IO.puts()
  end
  
  defp hit_target?(x_vel, y_vel, x_min, x_max, y_min, y_max) do
    simulate(0, 0, x_vel, y_vel, x_min, x_max, y_min, y_max)
  end
  
  defp simulate(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max) do
    cond do
      x_pos >= x_min and x_pos <= x_max and y_pos >= y_min and y_pos <= y_max ->
        true
      
      moving_away?(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max) ->
        false
      
      true ->
        new_x_pos = x_pos + x_vel
        new_y_pos = y_pos + y_vel
        
        new_x_vel = 
          cond do
            x_vel > 0 -> x_vel - 1
            x_vel < 0 -> x_vel + 1
            true -> 0
          end
        
        new_y_vel = y_vel - 1
        
        simulate(new_x_pos, new_y_pos, new_x_vel, new_y_vel, x_min, x_max, y_min, y_max)
    end
  end
  
  defp moving_away?(x_pos, y_pos, x_vel, y_vel, x_min, x_max, y_min, y_max) do
    (x_pos < x_min and x_vel < 0) or
    (x_pos > x_max and x_vel > 0) or
    (y_pos < y_min and y_vel < 0)
  end
end

Solution.solve()
