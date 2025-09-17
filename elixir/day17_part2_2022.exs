defmodule Solver do
  @chamber_width 7
  @profile_depth 30
  @rock_shapes [
    [{0,0},{1,0},{2,0},{3,0}],
    [{1,0},{0,1},{1,1},{2,1},{1,2}],
    [{0,0},{1,0},{2,0},{2,1},{2,2}],
    [{0,0},{0,1},{0,2},{0,3}],
    [{0,0},{1,0},{0,1},{1,1}]
  ]

  def read_input(filename) do
    File.read!(filename) |> String.trim()
  end

  def try_move(rock, dir, chamber) do
    {dx, dy} = case dir do
      "<" -> {-1, 0}
      ">" -> {1, 0}
      "v" -> {0, -1}
    end

    moved = Enum.map(rock, fn {x, y} -> {x + dx, y + dy} end)

    if Enum.all?(moved, fn {nx, ny} -> nx >= 0 and nx < @chamber_width and ny >= 1 end) and
       Enum.all?(moved, fn pos -> not MapSet.member?(chamber, pos) end) do
      moved
    else
      []
    end
  end

  def get_chamber_profile(chamber, highest_y) do
    lower = max(1, highest_y - @profile_depth)
    Enum.map(0..(@chamber_width - 1), fn x ->
      found_y = Enum.find_value((lower..highest_y) |> Enum.to_list() |> Enum.reverse(), fn y ->
        if MapSet.member?(chamber, {x, y}) do y end
      end)
      if found_y != nil, do: highest_y - found_y, else: (@profile_depth + 1)
    end)
  end

  def rock_highest_y(rock) do
    Enum.max_by(rock, fn {_x, y} -> y end) |> elem(1)
  end

  def solve(jet_pattern, total_rocks) do
    chamber = Enum.map(0..6, fn x -> {x, 0} end) |> MapSet.new()
    highest_y = 0
    jet_len = String.length(jet_pattern)
    jet_index = 0
    rock_index = 0
    rock_number = 0
    seen_states = %{}
    additional_height = 0

    solve_loop(chamber, highest_y, jet_pattern, jet_len, jet_index, rock_index, rock_number, seen_states, additional_height, total_rocks)
  end

  defp solve_loop(chamber, highest_y, jet_pattern, jet_len, jet_index, rock_index, rock_number, seen_states, additional_height, total_rocks) do
    shapes_len = length(@rock_shapes)
    shape = Enum.at(@rock_shapes, rem(rock_index, shapes_len))
    rock_x = 2
    rock_y_spawn = highest_y + 4
    current_rock = Enum.map(shape, fn {dx, dy} -> {rock_x + dx, rock_y_spawn + dy} end)

    {rock_final, jet_index_after} = drop_loop(current_rock, chamber, jet_pattern, jet_len, jet_index)

    new_chamber = Enum.reduce(rock_final, chamber, fn pos, acc -> MapSet.put(acc, pos) end
    )
    max_y_rock = rock_highest_y(rock_final)
    new_highest_y = if max_y_rock > highest_y, do: max_y_rock, else: highest_y

    profile = get_chamber_profile(new_chamber, new_highest_y)
    state = {rem(rock_index, shapes_len), rem(jet_index_after, jet_len), profile}

    {seen_states2, additional_height2, rock_number2, chamber2, highest_y2} =
      if Map.has_key?(seen_states, state) do
        {prev_rock_num, prev_height} = Map.get(seen_states, state)
        cycle_length = rock_number - prev_rock_num
        cycle_height = new_highest_y - prev_height
        remaining_rocks = total_rocks - rock_number
        if remaining_rocks > 0 do
          num_cycles = div(remaining_rocks, cycle_length)
          {
            seen_states,
            additional_height + num_cycles * cycle_height,
            rock_number + num_cycles * cycle_length,
            new_chamber,
            new_highest_y
          }
        else
          {seen_states, additional_height, rock_number, new_chamber, new_highest_y}
        end
      else
        new_seen = Map.put(seen_states, state, {rock_number, new_highest_y})
        {new_seen, additional_height, rock_number, new_chamber, new_highest_y}
      end

    rock_number_next = rock_number2 + 1
    rock_index_next = rock_index + 1

    if rock_number_next >= total_rocks do
      IO.puts(new_highest_y + additional_height2)
      :ok
    else
      solve_loop(chamber2, highest_y2, jet_pattern, jet_len, jet_index_after, rock_index_next, rock_number_next, seen_states2, additional_height2, total_rocks)
    end
  end

  defp drop_loop(current_rock, chamber, jet_pattern, jet_len, jet_index) do
    jet_char = String.at(jet_pattern, rem(jet_index, jet_len))
    moved_horizontal = case jet_char do
      ">" -> try_move(current_rock, ">", chamber)
      "<" -> try_move(current_rock, "<", chamber)
      _ -> []
    end

    rock_after_horiz = if moved_horizontal == [], do: current_rock, else: moved_horizontal
    moved_down = try_move(rock_after_horiz, "v", chamber)
    if moved_down != [] do
      drop_loop(moved_down, chamber, jet_pattern, jet_len, jet_index + 1)
    else
      {rock_after_horiz, jet_index + 1}
    end
  end

  def main() do
    jet_pattern = read_input("input.txt")
    total_rocks = 1_000_000_000_000
    solve(jet_pattern, total_rocks)
  end
end

Solver.main()