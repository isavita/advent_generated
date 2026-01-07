
defmodule Main do
  def main do
    grid =
      File.stream!("input.txt")
      |> Stream.map(&String.trim_trailing/1)
      |> Enum.reject(&(&1 == ""))

    if grid == [] do
      IO.puts(0)
      exit(:normal)
    end

    width = byte_size(hd(grid))
    {start_x, start_y} = find_start(grid, width)

    counts = %{start_x => 1}

    total =
      Enum.reduce(start_y..(length(grid) - 1), counts, fn y, acc ->
        Enum.reduce(acc, %{}, fn {x, cnt}, next ->
          char = :binary.at(Enum.at(grid, y), x)
          if char == ?^ do
            next
            |> Map.update(x - 1, cnt, &(&1 + cnt))
            |> Map.update(x + 1, cnt, &(&1 + cnt))
          else
            Map.update(next, x, cnt, &(&1 + cnt))
          end
        end)
      end)
      |> Map.values()
      |> Enum.sum()

    IO.puts(total)
  end

  defp find_start(grid, width) do
    grid
    |> Enum.with_index()
    |> Enum.find_value(fn {row, y} ->
      case :binary.match(row, "S") do
        {x, 1} -> {x, y}
        _ -> nil
      end
    end) || raise "Start point 'S' not found"
  end
end

Main.main()
