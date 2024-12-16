
defmodule Day20 do
  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end

  def parse_line(line) do
    ~r/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/
    |> Regex.run(line)
    |> case do
      [_, x, y, vx, vy] ->
        {%{x: String.to_integer(x), y: String.to_integer(y), vx: String.to_integer(vx), vy: String.to_integer(vy)}, :ok}
      _ ->
        {:error, "Invalid line format: #{line}"}
    end
  end

  def mod(a, b) do
    rem(a, b) + (if rem(a, b) < 0, do: b, else: 0)
  end

  def move_robots(robots, size_x, size_y) do
    Enum.map(robots, fn robot ->
      %{
        robot
        | x: mod(robot.x + robot.vx, size_x),
          y: mod(robot.y + robot.vy, size_y)
      }
    end)
  end

  def count_quadrants(robots, size_x, size_y) do
    center_x = div(size_x, 2)
    center_y = div(size_y, 2)

    robots
    |> Enum.reduce({0, 0, 0, 0}, fn robot, {q1, q2, q3, q4} ->
      cond do
        robot.x < center_x and robot.y < center_y -> {q1 + 1, q2, q3, q4}
        robot.x < center_x and robot.y > center_y -> {q1, q2 + 1, q3, q4}
        robot.x > center_x and robot.y < center_y -> {q1, q2, q3 + 1, q4}
        robot.x > center_x and robot.y > center_y -> {q1, q2, q3, q4 + 1}
        true -> {q1, q2, q3, q4}
      end
    end)
  end

  def has_no_overlaps?(robots) do
    robots
    |> Enum.map(fn robot -> {robot.x, robot.y} end)
    |> MapSet.new()
    |> MapSet.size() == length(robots)
  end

  def draw_grid(robots, size_x, size_y) do
    grid_map =
      robots
      |> Enum.map(fn robot -> {{robot.x, robot.y}, true} end)
      |> Map.new()

    0..(size_y - 1)
    |> Enum.each(fn y ->
      line =
        0..(size_x - 1)
        |> Enum.map(fn x ->
          if Map.get(grid_map, {x, y}), do: "#", else: "."
        end)
        |> Enum.join("")

      IO.puts(line)
    end)
  end

  def solve() do
    size_x = 101
    size_y = 103

    robots =
      case read_input("input.txt") do
        robots ->
          robots
          |> Enum.filter(fn {_, status} -> status == :ok end)
          |> Enum.map(fn {robot, _} -> robot end)
        _ ->
          IO.puts("Error reading input")
          System.halt(1)
      end

    # Part 1
    robots_part1 = robots
    robots_part1 =
      Enum.reduce(1..100, robots_part1, fn _, robots ->
        move_robots(robots, size_x, size_y)
      end)

    {q1, q2, q3, q4} = count_quadrants(robots_part1, size_x, size_y)
    safety_factor = q1 * q2 * q3 * q4
    IO.puts("Part 1 - Safety Factor after 100 seconds: #{safety_factor}")

    # Part 2
    robots_part2 = robots
    {seconds, robots_part2} =
      Enum.reduce(0..1_000_000, {0, robots_part2}, fn seconds, {acc_seconds, acc_robots} ->
        if has_no_overlaps?(acc_robots) do
          {:halt, {acc_seconds, acc_robots}}
        else
          {acc_seconds + 1, move_robots(acc_robots, size_x, size_y)}
        end
      end)

    if seconds > 1_000_000 do
      IO.puts("Exceeded maximum iterations without finding a unique position configuration.")
      System.halt(1)
    end

    IO.puts("Part 2 - Fewest seconds to display Easter egg: #{seconds}")
    IO.puts("Final positions of robots:")
    draw_grid(robots_part2, size_x, size_y)
  end
end

Day20.solve()
