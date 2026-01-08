
defmodule Main do
  defp read_grid do
    File.stream!("input.txt")
    |> Stream.map(&String.trim/1)
    |> Stream.reject(&(&1 == ""))
    |> Enum.map(&String.graphemes/1)
  end

  defp neighbours(r, c, rows, cols) do
    for dr <- -1..1,
        dc <- -1..1,
        dr != 0 or dc != 0,
        nr = r + dr,
        nc = c + dc,
        nr >= 0 and nr < rows and nc >= 0 and nc < cols,
        do: {nr, nc}
  end

  defp to_remove(grid, rows, cols) do
    for {row, r} <- Enum.with_index(grid),
        {cell, c} <- Enum.with_index(row),
        cell == "@",
        count =
          neighbours(r, c, rows, cols)
          |> Enum.count(fn {nr, nc} -> Enum.at(Enum.at(grid, nr), nc) == "@" end),
        count < 4,
        do: {r, c}
  end

  defp update_grid(grid, coords) do
    Enum.reduce(coords, grid, fn {r, c}, g ->
      List.update_at(g, r, &List.replace_at(&1, c, "."))
    end)
  end

  defp simulate(grid, total) do
    rows = length(grid)
    cols = length(hd(grid))

    case to_remove(grid, rows, cols) do
      [] -> total
      coords -> simulate(update_grid(grid, coords), total + length(coords))
    end
  end

  def main do
    case read_grid() do
      [] -> IO.puts("Total rolls removed: 0")
      grid -> IO.puts("Total rolls removed: #{simulate(grid, 0)}")
    end
  end
end

Main.main()
