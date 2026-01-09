
defmodule Solver do
  use Bitwise

  def main do
    grid = File.read!("input.txt")
           |> String.split("\n", trim: true)
           |> Enum.with_index()
           |> Enum.reduce(%{}, fn {line, y}, acc ->
             line |> String.to_charlist() |> Enum.with_index() |> Enum.reduce(acc, fn {char, x}, acc2 ->
               Map.put(acc2, {x, y}, char)
             end)
           end)

    {ox, oy} = Enum.find(grid, fn {_, v} -> v == ?@ end) |> elem(0)
    grid = grid
           |> Map.merge(%{
             {ox-1, oy-1} => ?@, {ox, oy-1} => ?#, {ox+1, oy-1} => ?@,
             {ox-1, oy}   => ?#, {ox, oy}   => ?#, {ox+1, oy}   => ?#,
             {ox-1, oy+1} => ?@, {ox, oy+1} => ?#, {ox+1, oy+1} => ?@
           })

    robots = grid |> Enum.filter(fn {_, v} -> v == ?@ end) |> Enum.map(&elem(&1, 0))
    keys_map = grid |> Enum.filter(fn {_, v} -> v >= ?a and v <= ?z end) |> Enum.into(%{}, fn {k, v} -> {v, k} end)
    all_keys_mask = (1 <<< Map.size(keys_map)) - 1

    starts = Enum.with_index(robots) |> Enum.into(%{}, fn {pos, i} -> {?0 + i, pos} end)
    nodes = Map.merge(starts, keys_map)

    graph = nodes |> Enum.into(%{}, fn {name, pos} ->
      {name, bfs(pos, grid)}
    end)

    initial_pos = {?0, ?1, ?2, ?3}
    IO.puts(dijkstra(:gb_sets.singleton({0, initial_pos, 0}), graph, all_keys_mask, %{}))
  end

  def bfs(start_pos, grid) do
    q = :queue.from_list([{start_pos, 0, 0}])
    visited = MapSet.new([start_pos])
    do_bfs(q, visited, grid, %{})
  end

  defp do_bfs(q, visited, grid, res) do
    case :queue.out(q) do
      {:empty, _} -> res
      {{:value, {{x, y}, dist, req}}, q} ->
        char = Map.get(grid, {x, y})
        {new_res, new_req} = cond do
          char >= ?a and char <= ?z and dist > 0 ->
            {Map.put(res, char, {dist, req}), req ||| (1 <<< (char - ?a))}
          char >= ?A and char <= ?Z ->
            {res, req ||| (1 <<< (char - ?A))}
          true -> {res, req}
        end

        {nq, nv} = Enum.reduce([{0,1}, {0,-1}, {1,0}, {-1,0}], {q, visited}, fn {dx, dy}, {acc_q, acc_v} ->
          np = {x + dx, y + dy}
          nc = Map.get(grid, np, ?#)
          if nc != ?# and not MapSet.member?(acc_v, np) do
            {:queue.in({np, dist + 1, new_req}, acc_q), MapSet.put(acc_v, np)}
          else
            {acc_q, acc_v}
          end
        end)
        do_bfs(nq, nv, grid, new_res)
    end
  end

  def dijkstra(pq, graph, target_mask, visited) do
    {{dist, pos_tuple, mask}, pq} = :gb_sets.take_smallest(pq)
    if mask == target_mask do
      dist
    else
      state = {pos_tuple, mask}
      if Map.get(visited, state, :infinity) <= dist do
        dijkstra(pq, graph, target_mask, visited)
      else
        visited = Map.put(visited, state, dist)
        new_pq = Enum.reduce(0..3, pq, fn i, acc_pq ->
          p_name = elem(pos_tuple, i)
          Enum.reduce(graph[p_name], acc_pq, fn {k, {d, req}}, acc_pq2 ->
            k_bit = 1 <<< (k - ?a)
            if (mask &&& k_bit) == 0 and (req &&& mask) == req do
              new_pos = put_elem(pos_tuple, i, k)
              new_dist = dist + d
              new_mask = mask ||| k_bit
              if Map.get(visited, {new_pos, new_mask}, :infinity) > new_dist do
                :gb_sets.add_element({new_dist, new_pos, new_mask}, acc_pq2)
              else
                acc_pq2
              end
            else
              acc_pq2
            end
          end)
        end)
        dijkstra(new_pq, graph, target_mask, visited)
      end
    end
  end
end

Solver.main()
