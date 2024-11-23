
defmodule CrabCups do
  def solve do
    {:ok, input} = File.read("input.txt")
    input = String.trim(input)

    cups = 
      input
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
      |> build_circle()

    result = play_game(cups, String.length(input), 100)
    print_result(result)
  end

  defp build_circle(cups) do
    max = Enum.max(cups)
    map = 
      cups
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {cup, i}, acc -> 
        Map.put(acc, cup, Enum.at(cups, rem(i + 1, length(cups))) || Enum.at(cups, 0))
      end)
    {map, Enum.at(cups, 0)}
  end

  defp play_game({cups, current}, max_cup, 0), do: {cups, current}
  defp play_game({cups, current}, max_cup, moves) do
    pickup1 = cups[current]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]

    next_current = cups[pickup3]
    cups = Map.put(cups, current, next_current)

    destination = find_destination(current, pickup1, pickup2, pickup3, max_cup)

    cups = cups
    |> Map.put(pickup3, cups[destination])
    |> Map.put(destination, pickup1)

    play_game({cups, next_current}, max_cup, moves - 1)
  end

  defp find_destination(current, p1, p2, p3, max_cup) do
    dest = if current > 1, do: current - 1, else: max_cup
    cond do
      dest != p1 and dest != p2 and dest != p3 -> dest
      true -> find_destination(dest, p1, p2, p3, max_cup)
    end
  end

  defp print_result({cups, _}) do
    Stream.unfold(cups[1], fn 
      1 -> nil
      x -> {x, cups[x]}
    end)
    |> Enum.join()
    |> IO.puts()
  end
end

CrabCups.solve()
