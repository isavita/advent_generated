
defmodule Main do
  def main do
    {:ok, data} = File.read("input.txt")
    lines = String.split(data, "\n", trim: false)

    {grid_lines, move_lines} =
      Enum.reduce_while(lines, {[], []}, fn
        "", {g, []} -> {:cont, {g, []}}
        "", {g, m} -> {:cont, {g, m}}
        line, {g, []} ->
          if String.contains?(line, ["#", "@", "O", "."]) do
            {:cont, {[line | g], []}}
          else
            {:cont, {Enum.reverse(g), [line]}}
          end

        line, {g, m} -> {:cont, {g, [line | m]}}
      end)

    grid = Enum.map(grid_lines, &String.to_charlist/1)
    rows = length(grid)
    cols = grid |> Enum.map(&length/1) |> Enum.max()

    padded_grid =
      Enum.map(grid, fn row ->
        row ++ List.duplicate(?\s, cols - length(row))
      end)

    {r0, c0} = find_robot(padded_grid)

    moves = move_lines |> Enum.reverse() |> Enum.join() |> String.to_charlist()

    {final_grid, _r, _c} =
      Enum.reduce(moves, {padded_grid, r0, c0}, fn
        ?^, {g, r, c} -> step(g, r, c, -1, 0)
        ?v, {g, r, c} -> step(g, r, c, 1, 0)
        ?<, {g, r, c} -> step(g, r, c, 0, -1)
        ?>, {g, r, c} -> step(g, r, c, 0, 1)
        _, acc -> acc
      end)

    sum =
      final_grid
      |> Enum.with_index()
      |> Enum.reduce(0, fn {row, r}, acc ->
        row
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {ch, c}, a ->
          if ch == ?O, do: a + r * 100 + c, else: a
        end)
      end)

    IO.puts(sum)
  end

  defp find_robot(grid) do
    Enum.reduce_while(Enum.with_index(grid), nil, fn {row, r}, _ ->
      case Enum.find_index(row, &(&1 == ?@)) do
        nil -> {:cont, nil}
        c -> {:halt, {r, c}}
      end
    end)
  end

  defp step(grid, r, c, dr, dc) do
    nr = r + dr
    nc = c + dc
    rows = length(grid)
    cols = length(List.first(grid))

    if nr < 0 or nr >= rows or nc < 0 or nc >= cols do
      {grid, r, c}
    else
      case get_cell(grid, nr, nc) do
        ?# -> {grid, r, c}
        ?O ->
          case push(grid, nr, nc, dr, dc) do
            {:ok, g2} -> move_robot(g2, r, c, nr, nc)
            :error -> {grid, r, c}
          end

        _ -> move_robot(grid, r, c, nr, nc)
      end
    end
  end

  defp move_robot(grid, r, c, nr, nc) do
    g1 = put_cell(grid, r, c, ?.)
    g2 = put_cell(g1, nr, nc, ?@)
    {g2, nr, nc}
  end

  defp push(grid, r, c, dr, dc) do
    nr = r + dr
    nc = c + dc
    rows = length(grid)
    cols = length(List.first(grid))

    if nr < 0 or nr >= rows or nc < 0 or nc >= cols do
      :error
    else
      case get_cell(grid, nr, nc) do
        ?# -> :error
        ?O ->
          case push(grid, nr, nc, dr, dc) do
            {:ok, g2} -> do_move_box(g2, r, c, nr, nc)
            :error -> :error
          end

        ?. -> do_move_box(grid, r, c, nr, nc)
        _ -> :error
      end
    end
  end

  defp do_move_box(grid, r, c, nr, nc) do
    g1 = put_cell(grid, nr, nc, ?O)
    g2 = put_cell(g1, r, c, ?.)
    {:ok, g2}
  end

  defp get_cell(grid, r, c), do: :lists.nth(c + 1, Enum.at(grid, r))
  defp put_cell(grid, r, c, val) do
    row = Enum.at(grid, r)
    new_row = List.replace_at(row, c, val)
    List.replace_at(grid, r, new_row)
  end
end

Main.main()
