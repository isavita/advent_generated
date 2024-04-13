defmodule Main do
  def run do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.reduce({%{x: 0, y: 0}, %{x: 0, y: 0}, %{}}, fn line, {head, tail, visited} ->
      [dir, steps] = String.split(line)
      num_steps = String.to_integer(steps)
      Enum.reduce(1..num_steps, {head, tail, visited}, fn _, {head, tail, visited} ->
        new_head = update_head(head, dir)
        {new_head, new_tail, new_visited} = update_tail(new_head, tail, visited)
        {new_head, new_tail, new_visited}
      end)
    end)
    |> elem(2)
    |> Map.keys()
    |> length()
    |> IO.puts()
  end

  defp update_head(%{x: x, y: y}, "R"), do: %{x: x + 1, y: y}
  defp update_head(%{x: x, y: y}, "L"), do: %{x: x - 1, y: y}
  defp update_head(%{x: x, y: y}, "U"), do: %{x: x, y: y + 1}
  defp update_head(%{x: x, y: y}, "D"), do: %{x: x, y: y - 1}

  defp update_tail(head, tail, visited) do
    if abs(head.x - tail.x) > 1 or abs(head.y - tail.y) > 1 do
      new_tail = if head.x != tail.x or head.y != tail.y do
        %{x: tail.x + sign(head.x - tail.x), y: tail.y + sign(head.y - tail.y)}
      else
        tail
      end
      new_visited = Map.put(visited, new_tail, true)
      {head, new_tail, new_visited}
    else
      new_visited = Map.put(visited, tail, true)
      {head, tail, new_visited}
    end
  end

  defp sign(x) when x > 0, do: 1
  defp sign(x) when x < 0, do: -1
  defp sign(0), do: 0
end

Main.run()