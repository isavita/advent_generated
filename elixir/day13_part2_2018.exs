
defmodule Day13 do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, acc ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, x}, acc2 ->
        Map.put(acc2, {x, y}, char)
      end)
    end)
  end

  defp initial_carts(tracks) do
    tracks
    |> Enum.filter(fn {_, char} -> char in [?<, ?>, ?^, ?v] end)
    |> Enum.map(fn {{x, y}, char} ->
      direction =
        case char do
          ?< -> :left
          ?> -> :right
          ?^ -> :up
          ?v -> :down
        end

      track_underneath =
        case char do
          ?< -> ?-
          ?> -> ?-
          ?^ -> ?|
          ?v -> ?|
        end
      
      {{x,y}, {direction, :left, track_underneath}}
    end)
    |> Enum.into(%{})
  end
  
  defp move({x, y}, direction) do
    case direction do
      :up -> {x, y - 1}
      :down -> {x, y + 1}
      :left -> {x - 1, y}
      :right -> {x + 1, y}
    end
  end

  defp turn(direction, track, next_turn) do
    case track do
      ?/ ->
        case direction do
          :up -> {:right, next_turn}
          :right -> {:up, next_turn}
          :down -> {:left, next_turn}
          :left -> {:down, next_turn}
        end

      ?\\ ->
        case direction do
          :up -> {:left, next_turn}
          :left -> {:up, next_turn}
          :down -> {:right, next_turn}
          :right -> {:down, next_turn}
        end

      ?+ ->
        new_direction =
          case {direction, next_turn} do
            {:up, :left} -> :left
            {:up, :straight} -> :up
            {:up, :right} -> :right
            {:down, :left} -> :right
            {:down, :straight} -> :down
            {:down, :right} -> :left
            {:left, :left} -> :down
            {:left, :straight} -> :left
            {:left, :right} -> :up
            {:right, :left} -> :up
            {:right, :straight} -> :right
            {:right, :right} -> :down
          end
        
        new_next_turn =
            case next_turn do
                :left -> :straight
                :straight -> :right
                :right -> :left
            end
        
        {new_direction, new_next_turn}
            

      _ -> {direction, next_turn} 
    end
  end
  
  defp tick(tracks, carts, part1 \\ false) do
      sorted_carts = carts |> Enum.sort_by(fn {{x, y}, _} -> {y, x} end)

      tick_helper(tracks, sorted_carts, %{}, nil, part1)
  end

  defp tick_helper(_tracks, [], carts, first_collision, _part1), do: {carts, first_collision}
    
  defp tick_helper(tracks, [{{x,y}, {direction,next_turn, _}}|rest], updated_carts, first_collision, part1) do
    
      carts = if Map.has_key?(updated_carts, {x,y}) do
                  Map.delete(updated_carts, {x,y})
              else
                  updated_carts
              end

      {new_x, new_y} = move({x, y}, direction)
      track = tracks[{new_x, new_y}]
      {new_direction, new_next_turn} = turn(direction, track, next_turn)

      if Map.has_key?(carts, {new_x, new_y}) or Enum.any?(rest, fn {{x2,y2},_} -> x2 == new_x and y2 == new_y end)  do
          if part1 do
            if first_collision == nil do
              tick_helper(tracks, rest, Map.delete(carts, {new_x,new_y}), {new_x, new_y}, part1)
            else
                tick_helper(tracks, rest, Map.delete(carts, {new_x,new_y}), first_collision, part1)
            end
          else
              
              remaining_carts = Map.delete(carts, {new_x, new_y}) |> Map.delete({x,y})
              new_rest = Enum.reject(rest, fn{{xx,yy},_} -> xx == new_x and yy == new_y end)
              tick_helper(tracks, new_rest, remaining_carts, first_collision, part1)
          end
      else
          new_track_underneath = if track == ?+, do: ?+, else: tracks[{new_x, new_y}]
          updated_carts = Map.put(carts, {new_x, new_y}, {new_direction, new_next_turn, new_track_underneath})
          
          
          tick_helper(tracks, rest, updated_carts, first_collision, part1)
      end
  end

  def part1(filename) do
    tracks = read_input(filename)
    carts = initial_carts(tracks)
    
    do_part1(tracks, carts)

  end
    
  defp do_part1(tracks,carts) do
    {new_carts, collision} = tick(tracks,carts, true)
    if collision != nil do
        "#{elem(collision,0)},#{elem(collision,1)}"
    else
        do_part1(tracks,new_carts)
    end
  end

  def part2(filename) do
    tracks = read_input(filename)
    carts = initial_carts(tracks)
    
    result_carts = do_part2(tracks, carts)
    {{x,y},_} = hd(Map.to_list(result_carts))
    "#{x},#{y}"
  end
    
  defp do_part2(tracks, carts) do
      {new_carts, _} = tick(tracks, carts, false)
      if map_size(new_carts) == 1 do
          new_carts
      else
          do_part2(tracks, new_carts)
      end
  end
end

# Run part 1
IO.puts("Part 1: #{Day13.part1("input.txt")}")

# Run part 2
IO.puts("Part 2: #{Day13.part2("input.txt")}")
