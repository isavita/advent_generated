
#!/usr/bin/env crystal

# Read the grid
grid = File.read_lines("input.txt").map(&.chomp)
rows = grid.size
cols = grid.first.size

# Find points of interest
poi_positions = [] of Tuple(Int32, Int32)
grid.each_with_index do |row, r|
  row.chars.each_with_index do |ch, c|
    if ('0' <= ch <= '9')
      poi_positions << {r, c}
    end
  end
end
poi_count = poi_positions.size
poi_index = Array(Int32).new(rows * cols, -1)
poi_positions.each_with_index { |pos, idx| poi_index[pos[0] * cols + pos[1]] = idx }

# BFS to compute distances from a start point to all POIs
def bfs(grid : Array(String), start : Tuple(Int32, Int32), poi_index : Array(Int32), poi_count : Int32)
  rows = grid.size
  cols = grid.first.size
  distances = Array(Int32).new(poi_count, -1)

  queue = [] of Tuple(Int32, Int32, Int32)
  visited = Array(Array(Bool)).new(rows) { Array(Bool).new(cols, false) }

  sr, sc = start
  queue << {sr, sc, 0}
  visited[sr][sc] = true

  dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]

  until queue.empty?
    r, c, d = queue.shift
    idx = poi_index[r * cols + c]
    distances[idx] = d if idx >= 0

    dirs.each do |dr, dc|
      nr = r + dr
      nc = c + dc
      next if nr < 0 || nr >= rows || nc < 0 || nc >= cols
      next if grid[nr][nc] == '#'
      next if visited[nr][nc]
      visited[nr][nc] = true
      queue << {nr, nc, d + 1}
    end
  end

  distances
end

# Build graph of distances between POIs
graph = Array(Array(Int32)).new(poi_count) { Array(Int32).new(poi_count, -1) }
poi_positions.each_with_index do |pos, idx|
  dists = bfs(grid, pos, poi_index, poi_count)
  poi_count.times { |j| graph[idx][j] = dists[j] }
end

# DP TSP with memoization
all_mask = (1 << poi_count) - 1
dp = Array(Array(Int32)).new(all_mask + 1) { Array(Int32).new(poi_count, -1) }

def tsp(mask : Int32, pos : Int32, graph : Array(Array(Int32)), dp : Array(Array(Int32)), all_mask : Int32)
  return graph[pos][0] if mask == all_mask
  return dp[mask][pos] if dp[mask][pos] != -1

  min_cost = Int32::MAX
  graph.size.times do |next_pos|
    next if (mask & (1 << next_pos)) != 0
    cost = graph[pos][next_pos] + tsp(mask | (1 << next_pos), next_pos, graph, dp, all_mask)
    min_cost = cost if cost < min_cost
  end

  dp[mask][pos] = min_cost
end

result = tsp(1, 0, graph, dp, all_mask)
puts result
