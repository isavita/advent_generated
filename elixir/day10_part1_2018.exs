defmodule StarAlign do
  def run do
    points =
      File.read!("input.txt")
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&parse_line/1)

    {message_time, message_positions} = find_message(points)

    IO.puts("Message appears after #{message_time} seconds:")
    print_message(message_positions)
  end

  defp parse_line(line) do
    [_, x, y, vx, vy] =
      Regex.run(
        ~r/position=<\s*([-]?\d+),\s*([-]?\d+)>\s*velocity=<\s*([-]?\d+),\s*([-]?\d+)>/,
        line
      )

    {{String.to_integer(x), String.to_integer(y)}, {String.to_integer(vx), String.to_integer(vy)}}
  end

  defp find_message(points, time \\ 0, previous_area \\ :infinity, previous_points \\ [])

  defp find_message(points, time, previous_area, previous_points) do
    updated_points = Enum.map(points, fn {{x, y}, {vx, vy}} -> {{x + vx, y + vy}, {vx, vy}} end)
    min_x = updated_points |> Enum.map(fn {{x, _}, _} -> x end) |> Enum.min()
    max_x = updated_points |> Enum.map(fn {{x, _}, _} -> x end) |> Enum.max()
    min_y = updated_points |> Enum.map(fn {{_, y}, _} -> y end) |> Enum.min()
    max_y = updated_points |> Enum.map(fn {{_, y}, _} -> y end) |> Enum.max()
    area = (max_x - min_x) * (max_y - min_y)

    if area < previous_area do
      find_message(updated_points, time + 1, area, updated_points)
    else
      {time, previous_points}
    end
  end

  defp print_message(points) do
    xs = Enum.map(points, fn {{x, _}, _} -> x end)
    ys = Enum.map(points, fn {{_, y}, _} -> y end)
    min_x = Enum.min(xs)
    max_x = Enum.max(xs)
    min_y = Enum.min(ys)
    max_y = Enum.max(ys)

    for y <- min_y..max_y do
      for x <- min_x..max_x do
        if Enum.any?(points, fn {{px, py}, _} -> px == x and py == y end) do
          IO.write("#")
        else
          IO.write(".")
        end
      end

      IO.puts("")
    end
  end
end

StarAlign.run()
