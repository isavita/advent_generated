
defmodule Main do
  def main do
    points =
      "input.txt"
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Stream.filter(&(&1 != ""))
      |> Stream.with_index()
      |> Enum.map(fn {line, id} ->
        [x, y, z] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
        {id, {x, y, z}}
      end)

    n = length(points)

    edges =
      for {i, {x1, y1, z1}} <- points,
          {j, {x2, y2, z2}} <- points,
          i < j,
          do: {
            (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2),
            i,
            j
          }

    sorted = Enum.sort_by(edges, &elem(&1, 0))
    limit = min(1000, length(sorted))

    {parent, size} =
      Enum.reduce(0..(n - 1), {%{}, %{}}, fn i, {p, s} ->
        {Map.put(p, i, i), Map.put(s, i, 1)}
      end)

    {parent, size} =
      Enum.reduce(0..(limit - 1), {parent, size}, fn idx, {p, s} ->
        {_d, u, v} = Enum.at(sorted, idx)
        {ru, p} = find(u, p)
        {rv, p} = find(v, p)
        if ru != rv do
          su = Map.get(s, ru)
          sv = Map.get(s, rv)
          if su < sv do
            {Map.put(p, ru, rv), Map.put(s, rv, su + sv)}
          else
            {Map.put(p, rv, ru), Map.put(s, ru, su + sv)}
          end
        else
          {p, s}
        end
      end)

    comps =
      Enum.reduce(0..(n - 1), %{}, fn i, acc ->
        {r, _p} = find(i, parent)
        Map.update(acc, r, 1, &(&1 + 1))
      end)
      |> Map.values()
      |> Enum.sort(:desc)
      |> Enum.take(3)

    result = Enum.reduce(comps, 1, &*/2)
    IO.puts("Product of three largest circuit sizes: #{result}")
  end

  defp find(i, parent) do
    p = Map.get(parent, i)
    if p == i do
      {i, parent}
    else
      {root, parent} = find(p, parent)
      {root, Map.put(parent, i, root)}
    end
  end
end

Main.main()
