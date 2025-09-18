
defmodule Grid do
  defstruct width: 0, height: 0, data: %{}
end

defmodule Edge do
  defstruct start: nil, end_coord: nil, weight: 0
end

defmodule Graph do
  defstruct vertices: MapSet.new(), edges: %{}
end

defmodule Solver do
  def main(_args) do
    lines = read_input("input.txt")
    IO.puts(solve(lines))
  end

  def read_input(file) do
    File.read!(file) |> String.split("\n", trim: true)
  end

  def solve(lines) do
    grid = parse_input(lines)
    start = {1, 0}
    end_coord = {grid.width - 2, grid.height - 1}
    graph = get_graph(grid, start, end_coord, &is_valid_neighbor/3)
    {_, max_dist} = dfs_max_distance(grid, graph, start, end_coord, MapSet.new())
    max_dist
  end

  def parse_input(lines) do
    height = length(lines)
    width = if height > 0, do: String.length(Enum.at(lines, 0)), else: 0

    data = Enum.reduce(0..(height - 1), %{}, fn y, acc ->
      line = Enum.at(lines, y)

      Enum.reduce(0..(width - 1), acc, fn x, acc2 ->
        ch = String.slice(line, x, 1)

        if ch != "." do
          Map.put(acc2, {x, y}, ch)
        else
          acc2
        end
      end)
    end)

    %Grid{width: width, height: height, data: data}
  end

  def is_in_bounds(coord, grid) do
    {x, y} = coord
    x >= 0 and y >= 0 and x < grid.width and y < grid.height
  end

  def is_valid_neighbor(grid, coord, _dir) do
    if not is_in_bounds(coord, grid) do
      false
    else
      Map.get(grid.data, coord, ".") != "#"
    end
  end

  defp add({x, y}, {dx, dy}), do: {x + dx, y + dy}

  def neighbors4(grid, coord, is_valid_neighbor_func) do
    dirs = [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]
    Enum.reduce(dirs, [], fn dir, acc ->
      nb = add(coord, dir)
      if is_valid_neighbor_func.(grid, nb, dir), do: acc ++ [nb], else: acc
    end)
  end

  def get_graph(grid, start, end_coord, is_valid_neighbor_func) do
    vertices = MapSet.new([start, end_coord])

    extra_lists =
      for y <- 0..(grid.height - 1) do
        for x <- 0..(grid.width - 1) do
          coord = {x, y}
          if Map.has_key?(grid.data, coord) do
            nil
          else
            neighs = neighbors4(grid, coord, is_valid_neighbor_func)
            if length(neighs) > 2, do: coord, else: nil
          end
        end
      end

    extra_coords = extra_lists |> List.flatten() |> Enum.filter(& &1)
    vertices2 = Enum.reduce(extra_coords, vertices, &MapSet.put(&2, &1))

    graph = %Graph{vertices: vertices2, edges: %{}}

    graph2 = Enum.reduce(graph.vertices, graph, fn vertex, g ->
      edges = get_edges_bfs(grid, vertex, graph.vertices, is_valid_neighbor_func)
      %{g | edges: Map.put(g.edges, vertex, edges)}
    end)

    graph2
  end

  def get_edges_bfs(grid, start, vertices, is_valid_neighbor_func) do
    frontier = [start]
    reached = MapSet.new([start])
    dist = %{start => 0}
    edges = []
    do_get_edges_bfs(grid, start, vertices, is_valid_neighbor_func, frontier, reached, dist, edges)
  end

  defp do_get_edges_bfs(_grid, _start, _vertices, _is_valid_neighbor_func, [], _reached, _dist, edges) do
    Enum.reverse(edges)
  end

  defp do_get_edges_bfs(grid, start, vertices, is_valid_neighbor_func, [current | rest], reached, dist, edges) do
    if MapSet.member?(vertices, current) and current != start do
      e = %Edge{start: start, end_coord: current, weight: Map.get(dist, current, 0)}
      do_get_edges_bfs(grid, start, vertices, is_valid_neighbor_func, rest, reached, dist, [e | edges])
    else
      current_dist = Map.get(dist, current, 0)
      nbrs = neighbors4(grid, current, is_valid_neighbor_func)

      {frontier2, reached2, dist2} =
        Enum.reduce(nbrs, {rest, reached, dist}, fn nxt, {f, r, d} ->
          if MapSet.member?(r, nxt) do
            {f, r, d}
          else
            {f ++ [nxt], MapSet.put(r, nxt), Map.put(d, nxt, current_dist + 1)}
          end
        end)

      do_get_edges_bfs(grid, start, vertices, is_valid_neighbor_func, frontier2, reached2, dist2, edges)
    end
  end

  def dfs_max_distance(grid, graph, current, end_coord, seen) do
    if current == end_coord do
      {true, 0}
    else
      seen2 = MapSet.put(seen, current)

      result =
        Enum.reduce(Map.get(graph.edges, current, []), {false, 0}, fn edge, {ok, best} ->
          if MapSet.member?(seen2, edge.end_coord) do
            {ok, best}
          else
            {valid, dist} = dfs_max_distance(grid, graph, edge.end_coord, end_coord, seen2)
            if valid do
              {true, Kernel.max(best, dist + edge.weight)}
            else
              {ok, best}
            end
          end
        end)

      {ok, best} = result

      if ok and best > 0 do
        {true, best}
      else
        {false, 0}
      end
    end
  end
end

Solver.main([])
