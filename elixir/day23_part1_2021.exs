
defmodule Solver do
  @targets %{?A => 3, ?B => 5, ?C => 7, ?D => 9}
  @costs %{?A => 1, ?B => 10, ?C => 100, ?D => 1000}
  @hallway [1, 2, 4, 6, 8, 10, 11]

  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        state = parse(content)
        max_r = state |> Map.keys() |> Enum.map(&elem(&1, 0)) |> Enum.max()
        IO.puts dijkstra(:gb_sets.singleton({0, state}), %{}, max_r)
      _ -> :ok
    end
  end

  def parse(input) do
    lines = String.split(input, "\n", trim: true)
    for {line, r} <- Enum.with_index(lines),
        {char, c} <- Enum.with_index(String.to_charlist(line)),
        char in ?A..?D,
        into: %{},
        do: {{r, c}, char}
  end

  def dijkstra(pq, visited, max_r) do
    {{cost, state}, pq} = :gb_sets.take_smallest(pq)
    cond do
      done?(state, max_r) -> cost
      Map.get(visited, state, :infinity) <= cost -> dijkstra(pq, visited, max_r)
      true ->
        visited = Map.put(visited, state, cost)
        pq = Enum.reduce(get_moves(state, max_r), pq, fn {m_cost, n_state}, acc ->
          if Map.get(visited, n_state, :infinity) > cost + m_cost do
            :gb_sets.add_element({cost + m_cost, n_state}, acc)
          else
            acc
          end
        end)
        dijkstra(pq, visited, max_r)
    end
  end

  def done?(state, max_r), do: Enum.all?(state, fn {p, t} -> settled?(p, t, state, max_r) end)

  def settled?({r, c}, type, state, max_r) do
    c == @targets[type] and Enum.all?(r..max_r, &(Map.get(state, {&1, c}) == type))
  end

  def get_moves(state, max_r) do
    tr_m = Enum.find_value(state, fn {pos, type} ->
      tc = @targets[type]
      if !settled?(pos, type, state, max_r) and clean?(state, type, tc, max_r) do
        tr = deepest(state, tc, max_r)
        if tr && clear?(state, pos, {tr, tc}) do
          {dist(pos, {tr, tc}) * @costs[type], state |> Map.delete(pos) |> Map.put({tr, tc}, type)}
        end
      end
    end)

    if tr_m, do: [tr_m], else: (
      for {p, t} <- state, elem(p, 0) > 1, !settled?(p, t, state, max_r),
          hc <- @hallway, clear?(state, p, {1, hc}) do
        {dist(p, {1, hc}) * @costs[t], state |> Map.delete(p) |> Map.put({1, hc}, t)}
      end
    )
  end

  def clean?(state, type, col, max_r) do
    Enum.all?(2..max_r, fn r -> (occ = state[{r, col}]) == nil or occ == type end)
  end

  def deepest(state, col, max_r) do
    2..max_r |> Enum.reject(&Map.has_key?(state, {&1, col})) |> List.last()
  end

  def clear?(state, {r1, c1}, {r2, c2}) do
    p1 = if r1 > 1, do: Enum.all?(1..(r1 - 1), &(!Map.has_key?(state, {&1, c1}))), else: true
    p2 = Enum.all?(c1..c2, fn c -> c == c1 or !Map.has_key?(state, {1, c}) end)
    p3 = if r2 > 1, do: Enum.all?(2..r2, &(!Map.has_key?(state, {&1, c2}))), else: true
    p1 and p2 and p3
  end

  def dist({r1, c1}, {r2, c2}), do: abs(r1 - 1) + abs(c1 - c2) + abs(r2 - 1)
end

Solver.main()
