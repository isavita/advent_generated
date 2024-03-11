defmodule StarAlign do
  def run do
    points =
      File.read!("input.txt")
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&parse_line/1)

    {message_time, _message_positions} = find_message(points)
    IO.puts(message_time)
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
end

StarAlign.run()
