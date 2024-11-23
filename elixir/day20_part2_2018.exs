
defmodule DoorMaze do
  def solve do
    {:ok, regex} = File.read("input.txt")
    regex = String.trim(regex)
    dm = build_map(String.slice(regex, 1..-2))
    rooms = count_rooms(dm, 1000)
    IO.puts(rooms)
  end

  def build_map(regex) do
    regex
    |> String.graphemes()
    |> Enum.reduce({%{}, {0, 0}, []}, fn 
      "(", {dm, cp, stack} -> 
        {dm, cp, [cp | stack]}
      "|", {dm, _cp, stack} -> 
        {dm, List.first(stack), stack}
      ")", {dm, _cp, stack} -> 
        {dm, List.first(stack), tl(stack)}
      dir, {dm, cp, stack} ->
        np = move(cp, dir)
        dm = Map.update(dm, cp, %{np => true}, fn doors -> Map.put(doors, np, true) end)
        {dm, np, stack}
    end)
    |> elem(0)
  end

  def move({x, y}, "N"), do: {x, y - 1}
  def move({x, y}, "S"), do: {x, y + 1}
  def move({x, y}, "E"), do: {x + 1, y}
  def move({x, y}, "W"), do: {x - 1, y}

  def count_rooms(dm, min_doors) do
    queue = [{0, 0}]
    visited = %{{0, 0} => 0}
    
    do_count_rooms(dm, queue, visited, 0, min_doors)
  end

  defp do_count_rooms(dm, [], visited, room_count, min_doors) do
    visited
    |> Enum.count(fn {_, dist} -> dist >= min_doors end)
  end

  defp do_count_rooms(dm, [p | queue], visited, room_count, min_doors) do
    doors = Map.get(dm, p, %{})
    
    {new_queue, new_visited} = 
      Enum.reduce(doors, {queue, visited}, fn {np, _}, {q, v} ->
        case Map.get(v, np) do
          nil -> 
            new_dist = Map.get(v, p, 0) + 1
            {[np | q], Map.put(v, np, new_dist)}
          _ -> 
            {q, v}
        end
      end)
    
    do_count_rooms(dm, new_queue, new_visited, room_count, min_doors)
  end
end

DoorMaze.solve()
