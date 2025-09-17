defmodule Solver do
  def rotate(current, :R), do: rem(current + 1, 4)
  def rotate(current, :L), do: rem(current - 1 + 4, 4)
  def points(d), do: rem(d + 3, 4)

  def cross_border({x, y}, dir, size) do
    s = size
    cond do
      x == -1 and y < 2 * s -> { {y + 2 * s, x + 1}, 1 }
      x == -1 and y >= 2 * s -> { {x + 4 * s, y - 2 * s}, 0 }
      x == s and dir == 2 -> { {y - s, x + s - 1}, 3 }
      x == 2 * s - 1 and dir == 0 -> { {y + s, x - s + 1}, 1 }
      x == 3 * s and dir == 2 -> { {y + 2 * s, x - 2 * s - 1}, 3 }
      x == 4 * s -> { {x - 4 * s, y + 2 * s}, 2 }
      y == -1 and x < 3 * s -> { {3 * s - 1 - x, y + s + 1}, 1}
      y == -1 and x >= 3 * s -> { {y + 1, x - 2 * s}, 2}
      y == s - 1 and x < s -> { {3 * s - 1 - x, y - s + 1}, 1}
      y == s - 1 and x >= s and dir == 3 -> { {y + s + 1, x - s}, 2}
      y == s and dir == 1 -> { {y + 2 * s - 1, x - 2 * s}, 0}
      y == 2 * s and x < 2 * s and dir == 1 -> { {y - s - 1, x + s}, 0}
      y == 2 * s and x >= 2 * s -> { {3 * s - 1 - x, y + s - 1}, 3}
      y == 3 * s -> { {3 * s - 1 - x, y - s - 1}, 3}
      true -> raise "Not a border crossing"
    end
  end

  def walk(map_data, dirs, size, pos, facing) do
    {dx, dy} = Enum.at(dirs, facing)
    nx = elem(pos, 0) + dx
    ny = elem(pos, 1) + dy
    next_pos = {nx, ny}
    case Map.fetch(map_data, next_pos) do
      {:ok, true} -> {pos, facing}
      {:ok, false} -> {next_pos, facing}
      :error ->
        {new_pos, new_facing} = cross_border(next_pos, facing, size)
        case Map.fetch(map_data, new_pos) do
          {:ok, true} -> {pos, facing}
          _ -> {new_pos, new_facing}
        end
    end
  end

  def parse_path(path) do
    parse_path_chars(String.graphemes(path), [], 0)
  end

  defp parse_path_chars([], movements, 0) do
    Enum.reverse(movements)
  end
  defp parse_path_chars([], movements, acc) do
    Enum.reverse([ {acc, nil} | movements ])
  end
  defp parse_path_chars([c | rest], movements, acc) when c in ["0","1","2","3","4","5","6","7","8","9"] do
    digit = String.to_integer(c)
    parse_path_chars(rest, movements, acc * 10 + digit)
  end
  defp parse_path_chars([c | rest], movements, acc) do
    rotate = case c do
      "R" -> :R
      "L" -> :L
      _ -> nil
    end
    new_movements = if acc != 0, do: [{acc, nil} | movements], else: movements
    parse_path_chars(rest, [{0, rotate} | new_movements], 0)
  end

  def parse_input(filename) do
    content = File.read!(filename)
    lines = String.split(content, "\n", trim: false)
    {map_lines, path_line} = split_map_and_path(lines)
    if path_line == nil do
      raise "No path line found"
    end
    size = div(String.length(hd(map_lines)), 3)

    map_data = Enum.with_index(map_lines)
    |> Enum.reduce(%{}, fn {line, r}, acc ->
      chars = String.graphemes(line)
      Enum.with_index(chars)
      |> Enum.reduce(acc, fn {ch, c}, m ->
        if ch == " " do
          m
        else
          Map.put(m, {r, c}, ch == "#")
        end
      end)
    end)

    movements = parse_path(String.trim(path_line))
    {map_data, size, movements}
  end

  def split_map_and_path(lines) do
    idx = lines |> Enum.find_index(fn l -> String.trim(l) == "" end)
    if idx == nil do
      {lines, nil}
    else
      map_lines = Enum.take(lines, idx)
      path_line = Enum.at(lines, idx + 1)
      {map_lines, path_line}
    end
  end

  def main do
    {map_data, size, movements} = parse_input("input.txt")
    dirs = [ {-1,0}, {0,1}, {1,0}, {0,-1} ]
    pos0 = {0, size}
    facing0 = 1

    {pos_final, facing_final} = Enum.reduce(movements, {pos0, facing0}, fn {steps, rot}, {cur_pos, cur_facing} ->
      facing_after = if rot != nil, do: rotate(cur_facing, rot), else: cur_facing
      {new_pos, new_facing} = perform_steps(steps, cur_pos, facing_after, map_data, dirs, size)
      {new_pos, new_facing}
    end)

    {cx, cy} = pos_final
    final = 1000 * (cx + 1) + 4 * (cy + 1) + points(facing_final)
    IO.puts(final)
  end

  def perform_steps(0, pos, _facing, _map_data, _dirs, _size), do: {pos, _facing}
  def perform_steps(n, pos, facing, map_data, dirs, size) do
    {new_pos, new_facing} = walk(map_data, dirs, size, pos, facing)
    if new_pos == pos and new_facing == facing do
      {pos, facing}
    else
      perform_steps(n - 1, new_pos, new_facing, map_data, dirs, size)
    end
  end
end

Solver.main()