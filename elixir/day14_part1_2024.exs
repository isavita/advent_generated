
defmodule Solver do
  def solve(path) do
    {width, height} = {101, 103}

    robots =
      path
      |> File.stream!()
      |> Stream.map(&parse_line/1)
      |> Enum.to_list()

    robots = simulate(robots, width, height, 100)

    {q1, q2, q3, q4} = count_quadrants(robots, width, height)

    q1 * q2 * q3 * q4
  end

  defp parse_line(line) do
    [p, v] =
      line
      |> String.split(" ")
      |> Enum.map(&String.trim/1)

    [px, py] =
      p
      |> String.trim_leading("p=")
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    [vx, vy] =
      v
      |> String.trim_leading("v=")
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    {px, py, vx, vy}
  end

  defp simulate(robots, width, height, steps) do
    Enum.reduce(0..(steps - 1), robots, fn _, robots_acc ->
      Enum.map(robots_acc, fn {px, py, vx, vy} ->
        x = Integer.mod(px + vx, width)
        y = Integer.mod(py + vy, height)
        {x, y, vx, vy}
      end)
    end)
  end

  defp count_quadrants(robots, width, height) do
     Enum.reduce(robots, {0,0,0,0}, fn {x, y, _, _}, {q1, q2, q3, q4} ->
      cond do
        x == div(width,2) or y == div(height, 2) -> {q1, q2, q3, q4}
        x < div(width, 2) and y < div(height, 2) -> {q1 + 1, q2, q3, q4}
        x > div(width, 2) and y < div(height, 2) -> {q1, q2 + 1, q3, q4}
        x < div(width, 2) and y > div(height, 2) -> {q1, q2, q3 + 1, q4}
        x > div(width, 2) and y > div(height, 2) -> {q1, q2, q3, q4 + 1}
      end
    end)
  end
end

IO.puts(Solver.solve("input.txt"))
