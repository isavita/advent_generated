
defmodule CubeConundrum do
  def call do
    input = File.read!("input.txt")
    games = parse_input(input)
    {part1(games), part2(games)}
  end

  defp parse_input(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_game/1)
  end

  defp parse_game(line) do
    [id_part | data] = String.split(line, ": ", parts: 2)
    id = String.slice(id_part, 5..-1) |> String.to_integer()
    sets = String.split(data |> List.first(), "; ")
    %{id: id, sets: Enum.map(sets, &parse_set/1)}
  end

  defp parse_set(set) do
    Enum.reduce(String.split(set, ", "), %{"red" => 0, "green" => 0, "blue" => 0}, fn entry, acc ->
      [count, color] = String.split(entry, " ")
      Map.update!(acc, color, &(&1 + String.to_integer(count)))
    end)
  end

  defp part1(games) do
    Enum.filter(games, &possible?(&1.sets, %{"red" => 12, "green" => 13, "blue" => 14}))
    |> Enum.map(& &1.id)
    |> Enum.sum()
  end

  defp possible?(sets, bag) do
    Enum.all?(sets, fn set ->
      Enum.all?(Map.keys(set), fn color ->
        set[color] <= bag[color]
      end)
    end)
  end

  defp part2(games) do
    Enum.map(games, fn %{sets: sets} ->
      sets
      |> Enum.reduce(%{"red" => 0, "green" => 0, "blue" => 0}, fn set, acc ->
        Enum.map(acc, fn {color, count} ->
          {color, max(count, set[color])}
        end) |> Enum.into(%{})
      end)
      |> Map.values()
      |> Enum.reduce(1, &*/2)
    end)
    |> Enum.sum()
  end
end
