
defmodule Rope do
  defmodule Point do
    defstruct [:x, :y]
  end

  def main do
    input = File.read!("input.txt")
    IO.puts(visited(input, 10))
  end

  def visited(input, rope_len) do
    rope = List.duplicate(%Point{x: 0, y: 0}, rope_len)
    visited = MapSet.new([%Point{x: 0, y: 0}])

    input
    |> String.split("\n", trim: true)
    |> Enum.reduce({rope, visited}, fn line, {rope, visited} ->
      [dir, steps] = String.split(line, " ")
      steps = String.to_integer(steps)
      move(rope, dir, steps, visited)
    end)
    |> elem(1)
    |> MapSet.size()
  end

  defp move(rope, dir, steps, visited) do
    Enum.reduce(1..steps, {rope, visited}, fn _, {rope, visited} ->
      new_rope = move_head(rope, dir)
      new_visited = MapSet.put(visited, List.last(new_rope))
      {new_rope, new_visited}
    end)
  end

  defp move_head([head | tail], dir) do
    new_head = move_point(head, dir)
    new_tail = follow(new_head, tail)
    [new_head | new_tail]
  end

  defp follow(_, []), do: []
  defp follow(head, [tail | rest]) do
    new_tail = move_tail(head, tail)
    [new_tail | follow(new_tail, rest)]
  end

  defp move_point(%Point{x: x, y: y}, "U"), do: %Point{x: x, y: y + 1}
  defp move_point(%Point{x: x, y: y}, "D"), do: %Point{x: x, y: y - 1}
  defp move_point(%Point{x: x, y: y}, "L"), do: %Point{x: x - 1, y: y}
  defp move_point(%Point{x: x, y: y}, "R"), do: %Point{x: x + 1, y: y}

  defp move_tail(%Point{x: hx, y: hy}, %Point{x: tx, y: ty} = tail) do
    cond do
      abs(hx - tx) <= 1 and abs(hy - ty) <= 1 -> tail
      true -> %Point{x: tx + sign(hx - tx), y: ty + sign(hy - ty)}
    end
  end

  defp sign(x) when x > 0, do: 1
  defp sign(x) when x < 0, do: -1
  defp sign(0), do: 0
end

Rope.main()
