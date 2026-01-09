
defmodule Main do
  def main do
    lines = File.read!("input.txt") |> String.split("\n", trim: true)
    h = length(lines) - 2
    w = String.length(hd(lines)) - 2
    grid = for {line, y} <- Enum.with_index(lines),
               {char, x} <- Enum.with_index(String.to_charlist(line)),
               char not in [?., ?#], into: %{}, do: {{x, y}, char}

    t1 = walk(MapSet.new([{1, 0}]), {w, h + 1}, 0, grid, w, h)
    t2 = walk(MapSet.new([{w, h + 1}]), {1, 0}, t1, grid, w, h)
    t3 = walk(MapSet.new([{1, 0}]), {w, h + 1}, t2, grid, w, h)
    IO.puts t3
  end

  def walk(ps, target, t, grid, w, h) do
    if MapSet.member?(ps, target), do: t, else: (
      nt = t + 1
      nps = for {x, y} <- ps, {dx, dy} <- [{0,0},{1,0},{-1,0},{0,1},{0,-1}],
                nx = x + dx, ny = y + dy,
                (nx == 1 and ny == 0) or (nx == w and ny == h + 1) or
                (nx > 0 and nx <= w and ny > 0 and ny <= h and
                 grid[{Integer.mod(nx - 1 - nt, w) + 1, ny}] != ?> and
                 grid[{Integer.mod(nx - 1 + nt, w) + 1, ny}] != ?< and
                 grid[{nx, Integer.mod(ny - 1 - nt, h) + 1}] != ?v and
                 grid[{nx, Integer.mod(ny - 1 + nt, h) + 1}] != ?^),
                into: MapSet.new(), do: {nx, ny}
      walk(nps, target, nt, grid, w, h)
    )
  end
end

Main.main()

