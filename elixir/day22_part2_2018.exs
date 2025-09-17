
defmodule AoC do
  use Bitwise

  @geo_y_mult 16807
  @geo_x_mult 48271
  @cave_mod 20183
  @tool_none 1
  @tool_torch 2
  @tool_gear 4

  def solve(input) do
    {depth, tx, ty} = parse(input)
    max_x = max(tx * 8 + 10, tx + 1)
    max_y = max(ty * 8 + 10, ty + 1)
    grid = precompute(depth, tx, ty, max_x, max_y)
    search(grid, depth, tx, ty)
  end

  def parse(input) do
    lines = String.split(input, "\n", trim: true)
    depth = lines |> Enum.at(0) |> String.split(": ") |> List.last() |> String.to_integer()
    t_line = lines |> Enum.at(1) |> String.split(": ") |> List.last()
    [tx, ty] = String.split(t_line, ",") |> Enum.map(&String.to_integer/1)
    {depth, tx, ty}
  end

  def precompute(depth, tx, ty, max_x, max_y) do
    {rows, _} =
      Enum.reduce(0..max_y - 1, {[], []}, fn y, {rows_acc, prev_row} ->
        {row, _} =
          Enum.reduce(0..max_x - 1, {[], nil}, fn x, {row_acc, last_erosion} ->
            gi =
              cond do
                x == 0 and y == 0 -> 0
                x == tx and y == ty -> 0
                y == 0 -> x * @geo_y_mult
                x == 0 -> y * @geo_x_mult
                true ->
                  er_left = last_erosion || 0
                  er_up = Enum.at(prev_row || [], x) || 0
                  er_left * er_up
              end
            erosion = rem(gi + depth, @cave_mod)
            {row_acc ++ [erosion], erosion}
          end)
        {rows_acc ++ [row], row}
      end)

    rows
  end

  def region_type(grid, x, y) do
    erosion = Enum.at(Enum.at(grid, y), x)
    rem(erosion, 3)
  end

  def allowed(region_type) do
    case region_type do
      0 -> @tool_gear ||| @tool_torch
      1 -> @tool_gear ||| @tool_none
      2 -> @tool_torch ||| @tool_none
    end
  end

  def neighbors(grid, x, y, equip) do
    width = length(Enum.at(grid, 0))
    height = length(grid)
    [
      {1, 0},
      {0, 1},
      {-1, 0},
      {0, -1}
    ]
    |> Enum.flat_map(fn {dx, dy} ->
      nx = x + dx
      ny = y + dy
      if nx >= 0 and ny >= 0 and nx < width and ny < height do
        region = region_type(grid, nx, ny)
        mask = allowed(region)
        if (equip &&& mask) != 0 do
          [{nx, ny, equip, 1}, {nx, ny, equip ^^^ mask, 8}]
        else
          []
        end
      else
        []
      end
    end)
  end

  def search(grid, _depth, tx, ty) do
    initial = {0, 0, 0, @tool_torch}
    dist = %{{0, 0, @tool_torch} => 0}
    pq = [initial]
    max_xh = tx * 8
    max_yh = ty * 8
    loop(pq, dist, grid, tx, ty, max_xh, max_yh)
  end

  def loop(pq, dist, grid, tx, ty, max_xh, max_yh) do
    if pq == [] do
      0
    else
      best = Enum.min_by(pq, fn {time, _x, _y, _e} -> time end)
      {time, x, y, equip} = best
      pq = List.delete(pq, best)

      if x == tx and y == ty and equip == @tool_torch do
        time
      else
        cond do
          x > max_xh or y > max_yh ->
            loop(pq, dist, grid, tx, ty, max_xh, max_yh)

          time > Map.get(dist, {x, y, equip}, :infty) ->
            loop(pq, dist, grid, tx, ty, max_xh, max_yh)

          true ->
            neigh = neighbors(grid, x, y, equip)
            {pq2, dist2} =
              Enum.reduce(neigh, {pq, dist}, fn {nx, ny, nequip, cost}, {acc_pq, acc_dist} ->
                new_time = time + cost
                key = {nx, ny, nequip}
                old = Map.get(acc_dist, key, :infty)

                if new_time < old do
                  { [{new_time, nx, ny, nequip} | acc_pq], Map.put(acc_dist, key, new_time) }
                else
                  {acc_pq, acc_dist}
                end
              end)

            loop(pq2, dist2, grid, tx, ty, max_xh, max_yh)
        end
      end
    end
  end
end

defmodule Main do
  def main do
    input = File.read!("input.txt")
    |> String.trim_trailing("\n")
    ans = AoC.solve(input)
    IO.puts(ans)
  end
end

Main.main()
