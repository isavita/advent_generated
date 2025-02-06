
defmodule Solution do
  defp rockstr(),
    do: "####\n\n # \n###\n # \n\n  #\n  #\n###\n\n#\n#\n#\n#\n\n##\n##"

  defp parse_rocks() do
    rockstr()
    |> String.split("\n\n")
    |> Enum.map(fn rock ->
      rock
      |> String.split("\n")
      |> Enum.with_index()
      |> Enum.reduce(MapSet.new(), fn {line, y}, acc ->
        line
        |> String.to_charlist()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {char, x}, acc ->
          if char == ?# do
            MapSet.put(acc, {x, length(String.split(rock, "\n")) - 1 - y})
          else
            acc
          end
        end)
      end)
    end)
  end

  defp dir_from_byte(b) do
    case b do
      ?> -> {1, 0}
      ?< -> {-1, 0}
    end
  end

  defp collision?(grid, rock, {px, py}) do
    Enum.any?(rock, fn {x, y} ->
      nx = px + x
      ny = py + y
      MapSet.member?(grid, {nx, ny}) or nx < 0 or nx > 6
    end)
  end

  def solve() do
    jet_pattern =
      File.read!("input.txt")
      |> String.trim()
      |> String.to_charlist()

    rocks = parse_rocks()
    grid = for x <- 0..6, into: MapSet.new(), do: {x, 0}
    run(grid, jet_pattern, rocks, 0, 0, 0, 2022)
  end

  defp run(grid, jet_pattern, rocks, floor, jet_index, rock_index, limit) when rock_index == limit do
    floor
  end
  defp run(grid, jet_pattern, rocks, floor, jet_index, rock_index, limit) do
    curr_rock = Enum.at(rocks, rem(rock_index, length(rocks)))
    pos = {2, floor + 4}

    {new_grid, new_floor, new_jet_index} =
      drop_rock(grid, jet_pattern, curr_rock, pos, jet_index, floor)

    run(new_grid, jet_pattern, rocks, new_floor, new_jet_index, rock_index + 1, limit)
  end

  defp drop_rock(grid, jet_pattern, curr_rock, pos, jet_index, floor) do
      {pos, jet_index} = move_horizontal(grid, jet_pattern, curr_rock, pos, jet_index)
      {pos, stop} = move_down(grid,curr_rock, pos)
      if stop do
        place_rock(grid, curr_rock, pos, floor, jet_index)
      else
        drop_rock(grid, jet_pattern, curr_rock, pos, jet_index, floor)
      end
  end

  defp move_horizontal(grid, jet_pattern, rock, pos, jet_index) do
    jet = Enum.at(jet_pattern, jet_index)
    new_jet_index = rem(jet_index + 1, length(jet_pattern))
    dir = dir_from_byte(jet)
    new_pos = {elem(pos, 0) + elem(dir, 0), elem(pos, 1) + elem(dir, 1)}

    if collision?(grid, rock, new_pos) do
      {pos, new_jet_index}
    else
      {new_pos, new_jet_index}
    end
  end

  defp move_down(grid, rock, pos) do
    new_pos = {elem(pos, 0), elem(pos, 1) -1}
    if collision?(grid, rock, new_pos) do
      {pos, true}
    else
      {new_pos, false}
    end
  end

  defp place_rock(grid, rock, pos, floor, jet_index) do
      new_grid = Enum.reduce(rock, grid, fn {x,y}, acc ->
        MapSet.put(acc, {x + elem(pos,0), y + elem(pos,1)})
      end)
      new_floor = Enum.reduce(rock, floor, fn {x,y}, acc ->
        max(acc, y + elem(pos, 1))
      end)
      {new_grid, new_floor, jet_index}
  end
end

Solution.solve()
|> IO.puts()
