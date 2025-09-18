defmodule Solver do
  def main(_args) do
    solve()
  end

  def solve() do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)
    grid = Enum.map(lines, &String.to_charlist/1)
    rows = length(grid)
    cols = length(Enum.at(grid, 0))
    {start_r, start_c, end_r, end_c} = find_positions(grid, rows, cols)
    visited = MapSet.new()
    queue = [{0, start_r, start_c, 0}]
    loop(queue, visited, end_r, end_c, grid, rows, cols)
  end

  def loop(queue, visited, end_r, end_c, grid, rows, cols) do
    case queue do
      [] -> :no_solution
      _ ->
        {best, rest} = extract_min(queue)
        {cost, r, c, dir} = best
        key = {r, c, dir}
        if MapSet.member?(visited, key) do
          loop(rest, visited, end_r, end_c, grid, rows, cols)
        else
          visited2 = MapSet.put(visited, key)
          if r == end_r and c == end_c do
            IO.puts(cost)
            :ok
          else
            neighbors = get_neighbors(grid, rows, cols, r, c, dir)
            new_queue = Enum.reduce(neighbors, rest, fn {nr, nc, ndir, move}, acc ->
              [{cost + move, nr, nc, ndir} | acc]
            end)
            loop(new_queue, visited2, end_r, end_c, grid, rows, cols)
          end
        end
    end
  end

  def extract_min(queue) do
    min_item = Enum.min_by(queue, fn x -> elem(x, 0) end)
    rest = List.delete(queue, min_item)
    {min_item, rest}
  end

  def get_neighbors(grid, rows, cols, r, c, dir) do
    dirs = [{0,1}, {1,0}, {0,-1}, {-1,0}]
    {dr, dc} = Enum.at(dirs, dir)
    nr = r + dr
    nc = c + dc
    forward_ok = nr >= 0 and nr < rows and nc >= 0 and nc < cols
    forward_allowed = forward_ok and Enum.at(Enum.at(grid, nr), nc) != ?#
    neighbors = if forward_allowed, do: [{nr, nc, dir, 1}], else: []
    new_dir1 = rem(dir + 1, 4)
    new_dir2 = rem(dir - 1 + 4, 4)
    neighbors = neighbors ++ [{r, c, new_dir1, 1000}, {r, c, new_dir2, 1000}]
    neighbors
  end

  def find_positions(grid, rows, cols) do
    Enum.reduce(0..(rows-1), {-1, -1, -1, -1}, fn r, acc ->
      {sr, sc, er, ec} = acc
      row = Enum.at(grid, r)
      Enum.reduce(0..(cols-1), {sr, sc, er, ec}, fn c, acc2 ->
        {sr2, sc2, er2, ec2} = acc2
        cell = Enum.at(row, c)
        cond do
          cell == ?S -> {r, c, er2, ec2}
          cell == ?E -> {sr2, sc2, r, c}
          true -> acc2
        end
      end)
    end)
  end
end

Solver.main([])