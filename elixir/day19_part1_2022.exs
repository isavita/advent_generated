
defmodule Solution do
  def main do
    case File.read("input.txt") do
      {:ok, data} ->
        blueprints = parse(data)
        ans = Enum.map(blueprints, fn b -> b.id * solve(b) end) |> Enum.sum()
        IO.puts "The sum of the quality levels is: #{ans}"
      _ -> :ok
    end
  end

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [id, o, c, ob1, ob2, g1, g2] = Regex.scan(~r/\d+/, line) |> List.flatten() |> Enum.map(&String.to_integer/1)
      %{id: id, o: o, c: c, ob: {ob1, ob2}, g: {g1, g2}, mo: Enum.max([o, c, ob1, g1])}
    end)
  end

  def solve(b) do
    {res, _} = dfs(24, 0, 0, 0, 1, 0, 0, b, %{})
    res
  end

  defp dfs(0, _, _, _, _, _, _, _, memo), do: {0, memo}
  defp dfs(t, o, c, ob, r1, r2, r3, b, memo) do
    st = {t, o, c, ob, r1, r2, r3}
    case memo[st] do
      nil ->
        tr = t - 1
        {res, memo} = cond do
          o >= elem(b.g, 0) and ob >= elem(b.g, 1) ->
            {v, m} = dfs(tr, min(o+r1-elem(b.g, 0), tr*b.mo), min(c+r2, tr*elem(b.ob, 1)), min(ob+r3-elem(b.g, 1), tr*elem(b.g, 1)), r1, r2, r3, b, memo)
            {v + tr, m}
          true ->
            {res, m} = dfs(tr, min(o+r1, tr*b.mo), min(c+r2, tr*elem(b.ob, 1)), min(ob+r3, tr*elem(b.g, 1)), r1, r2, r3, b, memo)
            {res, m} = if o >= b.o and r1 < b.mo do
              {v, m2} = dfs(tr, min(o+r1-b.o, tr*b.mo), min(c+r2, tr*elem(b.ob, 1)), min(ob+r3, tr*elem(b.g, 1)), r1+1, r2, r3, b, m)
              {max(res, v), m2}
            else {res, m} end
            {res, m} = if o >= b.c and r2 < elem(b.ob, 1) do
              {v, m2} = dfs(tr, min(o+r1-b.c, tr*b.mo), min(c+r2, tr*elem(b.ob, 1)), min(ob+r3, tr*elem(b.g, 1)), r1, r2+1, r3, b, m)
              {max(res, v), m2}
            else {res, m} end
            {res, m} = if o >= elem(b.ob, 0) and c >= elem(b.ob, 1) and r3 < elem(b.g, 1) do
              {v, m2} = dfs(tr, min(o+r1-elem(b.ob, 0), tr*b.mo), min(c+r2-elem(b.ob, 1), tr*elem(b.ob, 1)), min(ob+r3, tr*elem(b.g, 1)), r1, r2, r3+1, b, m)
              {max(res, v), m2}
            else {res, m} end
            {res, m}
        end
        {res, Map.put(memo, st, res)}
      v -> {v, memo}
    end
  end
end

Solution.main()
