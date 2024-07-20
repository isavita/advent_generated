
defmodule AsteroidBelt do
  def run do
    input = File.read!("input.txt") |> String.trim()
    asteroids = parse_input(input)
    best_location = find_best_location(asteroids)
    IO.puts("Best location: #{inspect(best_location)}")
    
    vaporization_order = vaporize_asteroids(asteroids, best_location)
    asteroid_200 = Enum.at(vaporization_order, 199)
    
    result = elem(asteroid_200, 0) * 100 + elem(asteroid_200, 1)
    IO.puts("200th asteroid vaporized: #{inspect(asteroid_200)}")
    IO.puts("Result: #{result}")
  end

  defp parse_input(input) do
    input
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, y} ->
      String.graphemes(line)
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> char == "#" end)
      |> Enum.map(fn {_, x} -> {x, y} end)
    end)
  end

  defp find_best_location(asteroids) do
    asteroids
    |> Enum.map(&{&1, count_visible_asteroids(&1, asteroids)})
    |> Enum.max_by(fn {_, count} -> count end)
    |> elem(0)
  end

  defp count_visible_asteroids({x1, y1}, asteroids) do
    asteroids
    |> Enum.reject(&(&1 == {x1, y1}))
    |> Enum.map(&calculate_angle({x1, y1}, &1))
    |> Enum.uniq()
    |> length()
  end

  defp calculate_angle({x1, y1}, {x2, y2}) do
    dx = x2 - x1
    dy = y2 - y1
    :math.atan2(dy, dx)
  end

  defp vaporize_asteroids(asteroids, station) do
    asteroids
    |> Enum.reject(&(&1 == station))
    |> Enum.group_by(&calculate_angle(station, &1))
    |> Enum.map(fn {angle, coords} -> {angle, Enum.sort_by(coords, &distance(station, &1))} end)
    |> Enum.sort_by(&elem(&1, 0))
    |> vaporize_in_order(station)
  end

  defp vaporize_in_order(groups, station) do
    vaporized = []
    angles = Enum.map(groups, &elem(&1, 0))

    loop_vaporization(groups, angles, vaporized, station)
  end

  defp loop_vaporization(groups, angles, vaporized, station) do
    if Enum.empty?(angles) do
      vaporized
    else
      for angle <- angles do
        case Map.get(groups, angle) do
          [next | rest] ->
            vaporized = vaporized ++ [next]
            groups = Map.put(groups, angle, rest)
          _ -> :ok
        end
      end
      loop_vaporization(groups, angles, vaporized, station)
    end
  end

  defp distance({x1, y1}, {x2, y2}) do
    :math.sqrt(:math.pow(x2 - x1, 2) + :math.pow(y2 - y1, 2))
  end
end

AsteroidBelt.run()
