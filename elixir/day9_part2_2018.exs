
defmodule MarbleGame do
  def main do
    {players, last_marble} = read_input("input.txt")
    last_marble = last_marble * 100
    IO.puts(play(players, last_marble))
  end

  defp read_input(file) do
    [line] = File.read!(file) |> String.split("\n", trim: true)
    parts = String.split(line)
    {String.to_integer(Enum.at(parts, 0)),
     String.to_integer(Enum.at(parts, 6))}
  end

  defp play(players, last) do
    scores = :array.new(players, default: 0)

    # circular linked list stored as two maps: next and prev
    next = %{0 => 0}
    prev = %{0 => 0}
    state = %{cur: 0, next: next, prev: prev, scores: scores}

    Enum.reduce(1..last, state, fn marble, acc ->
      if rem(marble, 23) == 0 do
        player = rem(marble, players)

        # move 7 steps counter‑clockwise
        cur = Enum.reduce(1..7, acc.cur, fn _, c -> Map.fetch!(acc.prev, c) end)

        # add points
        scores = :array.set(player, :array.get(player, acc.scores) + marble + cur, acc.scores)

        # remove cur from the circle
        p = Map.fetch!(acc.prev, cur)
        n = Map.fetch!(acc.next, cur)
        next = Map.put(acc.next, p, n)
        prev = Map.put(acc.prev, n, p)

        %{acc | cur: n, next: next, prev: prev, scores: scores}
      else
        # move one step clockwise
        cur = Map.fetch!(acc.next, acc.cur)

        # insert new marble after cur
        n = Map.fetch!(acc.next, cur)

        next = acc.next |> Map.put(cur, marble) |> Map.put(marble, n)
        prev = acc.prev |> Map.put(marble, cur) |> Map.put(n, marble)

        %{acc | cur: marble, next: next, prev: prev}
      end
    end)
    |> max_score()
  end

  defp max_score(%{scores: scores}) do
    0..(:array.size(scores) - 1)
    |> Enum.map(& :array.get(&1, scores))
    |> Enum.max()
  end
end

MarbleGame.main()
