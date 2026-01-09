
defmodule Amphipod do
  @target_cols %{?A => 3, ?B => 5, ?C => 7, ?D => 9}
  @costs %{?A => 1, ?B => 10, ?C => 100, ?D => 1000}
  @hallway_cols [1, 2, 4, 6, 8, 10, 11]
  @room_rows [2, 3, 4, 5]

  def main do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)
    lines = [
      Enum.at(lines, 0),
      Enum.at(lines, 1),
      Enum.at(lines, 2),
      "  #D#C#B#A#",
      "  #D#B#A#C#",
      Enum.at(lines, 3),
      Enum.at(lines, 4)
    ]
    grid = for {line, r} <- Enum.with_index(lines),
               {char, c} <- Enum.with_index(String.to_charlist(line)),
               char in ?A..?D,
               into: %{}, do: {{r, c}, char}
    IO.puts solve(grid)
  end

  def solve(grid) do
    pq = :gb_sets.singleton({0, grid})
    dijkstra(pq, MapSet.new())
  end

  def dijkstra(pq, seen) do
    case :gb_sets.take_smallest(pq) do
      {{energy, grid}, pq} ->
        cond do
          MapSet.member?(seen, grid) -> dijkstra(pq, seen)
          done?(grid) -> energy
          true ->
            seen = MapSet.put(seen, grid)
            new_pq = Enum.reduce(get_moves(grid), pq, fn {pos, target, type}, acc ->
              dist = abs(elem(pos, 1) - elem(target, 1)) + (elem(pos, 0) - 1) + (elem(target, 0) - 1)
              new_grid = grid |> Map.delete(pos) |> Map.put(target, type)
              :gb_sets.add_element({energy + dist * @costs[type], new_grid}, acc)
            end)
            dijkstra(new_pq, seen)
        end
    end
  end

  def done?(grid), do: Enum.all?(grid, fn {pos, type} -> settled?(grid, pos, type) end)

  def settled?(grid, {r, c}, type) do
    r > 1 and c == @target_cols[type] and Enum.all?(@room_rows, fn rr -> rr <= r or grid[{rr, c}] == type end)
  end

  def get_moves(grid) do
    to_room = for {pos, type} <- grid, !settled?(grid, pos, type), t = can_move_to_room(grid, pos, type), t, do: {pos, t, type}
    if to_room != [], do: to_room, else: (for {p, type} <- grid, !settled?(grid, p, type), elem(p, 0) > 1, c <- @hallway_cols, path_clear?(grid, p, {1, c}), do: {p, {1, c}, type})
  end

  def can_move_to_room(grid, {r, c}, type) do
    tc = @target_cols[type]
    if Enum.all?(@room_rows, fn rr -> grid[{rr, tc}] in [nil, type] end) do
      tr = Enum.find(Enum.reverse(@room_rows), fn rr -> is_nil(grid[{rr, tc}]) end)
      if tr && path_clear?(grid, {r, c}, {tr, tc}), do: {tr, tc}
    end
  end

  def path_clear?(grid, {r1, c1}, {r2, c2}) do
    h_range = cond do
      c1 < c2 -> (c1+1)..c2
      c1 > c2 -> (c1-1)..c2
      true -> []
    end
    v1 = if r1 > 1, do: for(rr <- 1..(r1-1), do: {rr, c1}), else: []
    h = for cc <- h_range, do: {1, cc}
    v2 = if r2 > 1, do: for(rr <- 2..r2, do: {rr, c2}), else: []
    path = Enum.reject(v1 ++ h ++ v2, &(&1 == {r1, c1}))
    Enum.all?(path, &is_nil(grid[&1]))
  end
end

Amphipod.main()

