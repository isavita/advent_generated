
defmodule Solution do
  def main do
    clay = File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.reduce(MapSet.new(), fn line, acc ->
      [a, v, v1, v2] = Regex.run(~r/([xy])=(\d+), [xy]=(\d+)\.\.(\d+)/, line, capture: :all_but_first)
      {v, v1, v2} = {String.to_integer(v), String.to_integer(v1), String.to_integer(v2)}
      if a == "x", do: Enum.reduce(v1..v2, acc, &MapSet.put(&2, {v, &1})),
                  else: Enum.reduce(v1..v2, acc, &MapSet.put(&2, {&1, v}))
    end)

    {min_y, max_y} = clay |> Enum.map(fn {_, y} -> y end) |> Enum.min_max()
    {water, _} = flow({500, 0}, clay, %{}, max_y)
    
    IO.puts Enum.count(water, fn {{_, y}, _} -> y >= min_y and y <= max_y end)
  end

  defp flow({_, y}, _, w, max) when y > max, do: {w, :spill}
  defp flow(p, clay, w, max) do
    case Map.get(w, p) do
      :settle -> {w, :settle}
      :flow -> {w, :spill}
      nil ->
        if MapSet.member?(clay, p), do: {w, :clay},
        else: (
          {w, res} = flow(down(p), clay, Map.put(w, p, :flow), max)
          if res in [:clay, :settle], do: fill_row(p, clay, w, max), else: {w, :spill}
        )
    end
  end

  defp fill_row({x, y}, clay, w, max) do
    {w, r_res, r_x} = side(x + 1, y, 1, clay, w, max)
    {w, l_res, l_x} = side(x - 1, y, -1, clay, w, max)
    type = if r_res == :clay and l_res == :clay, do: :settle, else: :flow
    {Enum.reduce(l_x..r_x, w, &Map.put(&2, {&1, y}, type)), type}
  end

  defp side(x, y, dx, clay, w, max) do
    p = {x, y}
    cond do
      MapSet.member?(clay, p) -> {w, :clay, x - dx}
      not solid?(down(p), clay, w) ->
        {w, res} = flow(p, clay, w, max)
        if res == :settle, do: side(x + dx, y, dx, clay, w, max), else: {w, :spill, x}
      true -> side(x + dx, y, dx, clay, w, max)
    end
  end

  defp solid?(p, clay, w), do: MapSet.member?(clay, p) or Map.get(w, p) == :settle
  defp down({x, y}), do: {x, y + 1}
end

Solution.main()
