
defmodule Day19 do
  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
    |> then(&process_grid/1)
    |> IO.puts()
  end

  defp process_grid(grid) do
    x = Enum.find_index(grid |> hd(), &(&1 == ?|))
    {0, x, 0, 1, 0}
    |> walk(grid)
  end

  defp walk({y, x, dx, dy, steps}, grid) do
    case get_cell(grid, y, x) do
      nil -> steps
      ?  -> steps
      ?+ ->
        cond do
          dx == 0 ->
            if get_cell(grid, y, x - 1) in [?-, ?A..?Z] do
              walk({y, x - 1, -1, 0, steps + 1}, grid)
            else
              walk({y, x + 1, 1, 0, steps + 1}, grid)
            end
          true ->
            if get_cell(grid, y - 1, x) in [?|, ?A..?Z] do
              walk({y - 1, x, 0, -1, steps + 1}, grid)
            else
              walk({y + 1, x, 0, 1, steps + 1}, grid)
            end
        end
      _  -> walk({y + dy, x + dx, dx, dy, steps + 1}, grid)
    end
  end

  defp get_cell(grid, y, x) when y < 0 or y >= length(grid), do: nil
  defp get_cell(grid, y, x) when x < 0 or x >= length(hd(grid)), do: nil
  defp get_cell(grid, y, x), do: Enum.at(Enum.at(grid, y), x)
end

Day19.solve()
