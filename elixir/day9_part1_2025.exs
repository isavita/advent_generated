
defmodule Main do
  def run do
    points =
      File.stream!("input.txt")
      |> Stream.map(&String.trim/1)
      |> Stream.reject(&(&1 == ""))
      |> Stream.map(fn line ->
        case String.split(line, ",") do
          [a, b] ->
            case {Integer.parse(a), Integer.parse(b)} do
              {{x, ""}, {y, ""}} -> {x, y}
              _ -> nil
            end
          _ -> nil
        end
      end)
      |> Stream.reject(&(&1 == nil))
      |> Enum.to_list()

    max_area =
      for {x1, y1} <- points, {x2, y2} <- points, reduce: 0 do
        acc ->
          dx = abs(x1 - x2) + 1
          dy = abs(y1 - y2) + 1
          max(acc, dx * dy)
      end

    IO.puts("Largest area: #{max_area}")
  end
end

Main.run()
