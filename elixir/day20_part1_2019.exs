defmodule Solver do
  def solve(path) do
    lines = File.read!(path) |> String.split("\n", trim: true)
    m = parse(lines)
    bfs(m)
  end

  def parse(lines) do
    x_max = length(lines)
    y_max = lines |> Enum.map(&String.length/1) |> Enum.max()
    grid = build_grid(lines)

    acc0 = %{aa: nil, zz: nil, portal_name: %{}, teleport: %{}, is_outer: %{}, cache: %{}}
    acc = Enum.reduce(0..(x_max - 1), acc0, fn i, acc_i ->
      line = Enum.at(lines, i)
      Enum.reduce(0..(y_max - 1), acc_i, fn j, acc_j ->
        c = Map.get(grid, {i, j})
        if letter?(c) do
          {name, pPoint, ok} = extract_portal(grid, {i, j})
          if ok do
            portal_name_map = Map.put(acc_j.portal_name, pPoint, name)
            acc_j = %{acc_j | portal_name: portal_name_map}

            acc_j =
              case name do
                "AA" ->
                  acc_j = %{acc_j | aa: pPoint}
                  is_outer_map = Map.put(acc_j.is_outer, pPoint, true)
                  %{acc_j | is_outer: is_outer_map}
                "ZZ" ->
                  acc_j = %{acc_j | zz: pPoint}
                  is_outer_map = Map.put(acc_j.is_outer, pPoint, true)
                  %{acc_j | is_outer: is_outer_map}
                _ ->
                  cache = acc_j.cache
                  if Map.has_key?(cache, name) do
                    target = Map.get(cache, name)
                    teleport = acc_j.teleport |> Map.put(pPoint, target) |> Map.put(target, pPoint)
                    %{acc_j | teleport: teleport}
                  else
                    cache = Map.put(cache, name, pPoint)
                    %{acc_j | cache: cache}
                  end
              end

            pX = elem(pPoint, 0)
            pY = elem(pPoint, 1)
            outer = pX == 0 or pY == 0 or pX == x_max - 2 or pY == y_max - 2
            is_outer = acc_j.is_outer
            is_outer = Map.put(is_outer, pPoint, outer)
            %{acc_j | is_outer: is_outer}
          else
            acc_j
          end
        else
          acc_j
        end
      end)
    end)

    %{
      x_max: x_max,
      y_max: y_max,
      grid: grid,
      aa: acc.aa,
      zz: acc.zz,
      teleport: acc.teleport,
      portal_name: acc.portal_name,
      is_outer: acc.is_outer
    }
  end

  def build_grid(lines) do
    Enum.reduce(Enum.with_index(lines), %{}, fn {line, i}, acc ->
      graphemes = String.graphemes(line)
      Enum.reduce(Enum.with_index(graphemes), acc, fn {ch, j}, acc2 ->
        Map.put(acc2, {i, j}, ch)
      end)
    end)
  end

  def extract_portal(grid, {x, y}) do
    c1 = Map.get(grid, {x, y})

    c2 = Map.get(grid, {x + 1, y})
    if letter?(c2) do
      portal_name = c1 <> c2
      portal_point = {x + 2, y}
      if Map.get(grid, portal_point) == "." do
        {portal_name, portal_point, true}
      else
        portal_point = {x - 1, y}
        if Map.get(grid, portal_point) == "." do
          {portal_name, portal_point, true}
        else
          {"", {0, 0}, false}
        end
      end
    else
      c2 = Map.get(grid, {x, y + 1})
      if letter?(c2) do
        portal_name = c1 <> c2
        portal_point = {x, y + 2}
        if Map.get(grid, portal_point) == "." do
          {portal_name, portal_point, true}
        else
          portal_point = {x, y - 1}
          if Map.get(grid, portal_point) == "." do
            {portal_name, portal_point, true}
          else
            {"", {0, 0}, false}
          end
        end
      else
        {"", {0, 0}, false}
      end
    end
  end

  def letter?("") do
    false
  end

  def letter?(nil) do
    false
  end

  def letter?(c) when is_binary(c) do
    String.length(c) == 1 and c >= "A" and c <= "Z"
  end

  def bfs(m) do
    discovered = MapSet.new([m.aa])
    to_do = [m.aa]
    depth = 0
    do_bfs(m, discovered, to_do, depth)
  end

  def do_bfs(_m, _discovered, [], depth) do
    -1
  end

  def do_bfs(m, discovered, to_do, depth) do
    {discovered2, next_to_do, found} =
      Enum.reduce(to_do, {discovered, [], false}, fn curr, {d, acc, f} ->
        if curr == m.zz do
          {d, acc, true}
        else
          neighs = neighbours(curr)
          Enum.reduce(neighs, {d, acc, f}, fn n, {d2, acc2, f2} ->
            dest = Map.get(m.grid, n)
            cond do
              dest == "#" or dest == nil ->
                {d2, acc2, f2}
              dest == "." ->
                if MapSet.member?(d2, n) do
                  {d2, acc2, f2}
                else
                  {MapSet.put(d2, n), acc2 ++ [n], f2}
                end
              is_letter?(dest) ->
                next = Map.get(m.teleport, curr)
                if next != nil and !MapSet.member?(d2, next) do
                  {MapSet.put(d2, next), acc2 ++ [next], f2}
                else
                  {d2, acc2, f2}
                end
              true ->
                {d2, acc2, f2}
            end
          end)
        end
      end)

    if found, do: depth, else: do_bfs(m, discovered2, next_to_do, depth + 1)
  end

  def neighbours({x, y}) do
    [{x, y + 1}, {x + 1, y}, {x, y - 1}, {x - 1, y}]
  end

  def is_letter?(dest) when is_binary(dest) do
    String.length(dest) == 1 and dest >= "A" and dest <= "Z"
  end

  def is_letter?(_) do
    false
  end
end

defmodule Main do
  def main do
    IO.puts(Solver.solve("input.txt"))
  end
end

Main.main()