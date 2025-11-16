
def solve(input_file, ignore_slopes)
  grid = File.read_lines(input_file).map(&.chars)
  rows = grid.size
  cols = grid[0].size
  start = {0, grid[0].index('.').not_nil!}
  end_pos = {rows - 1, grid[rows - 1].index('.').not_nil!}

  graph = build_graph(grid, ignore_slopes)
  find_longest_path(graph, start, end_pos)
end

def build_graph(grid, ignore_slopes)
  rows = grid.size
  cols = grid[0].size
  graph = Hash(String, Array(Tuple(Int32, Int32))).new
  directions = [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
  slopes = {'^' => {-1, 0}, 'v' => {1, 0}, '<' => {0, -1}, '>' => {0, 1}}

  rows.times do |r|
    cols.times do |c|
      next if grid[r][c] == '#'

      neighbors = [] of Tuple(Int32, Int32)
      directions.each do |dir|
        if !ignore_slopes && slopes.has_key?(grid[r][c]) && slopes[grid[r][c]] != dir
          next
        end

        nr = r + dir[0]
        nc = c + dir[1]

        if 0 <= nr < rows && 0 <= nc < cols && grid[nr][nc] != '#'
          neighbors << {nr, nc}
        end
      end
      graph[pos_to_key(r, c)] = neighbors
    end
  end
  compress_graph(graph)
end

def compress_graph(graph)
  compressed_graph = Hash(String, Hash(String, Int32)).new

  graph.each do |node, neighbors|
    compressed_graph[node] = Hash(String, Int32).new
    neighbors.each do |neighbor|
      dist = 1
      prev = key_to_pos(node)
      curr = neighbor

      while graph[pos_to_key(curr[0], curr[1])].size == 2
        next_neighbors = graph[pos_to_key(curr[0], curr[1])]
        next_node = (next_neighbors[0] == prev) ? next_neighbors[1] : next_neighbors[0]
        prev = curr
        curr = next_node
        dist += 1
      end
      compressed_graph[node][pos_to_key(curr[0], curr[1])] = dist
    end
  end

  compressed_graph
end

def find_longest_path(graph, start, end_pos)
  longest_path = 0
  start_key = pos_to_key(start[0], start[1])
  end_key = pos_to_key(end_pos[0], end_pos[1])
  stack = [{start_key, 0, Set(String).new}]

  while !stack.empty?
    curr, dist, visited = stack.pop

    if curr == end_key
      longest_path = Math.max(longest_path, dist)
      next
    end

    visited = visited.dup
    visited << curr

    graph[curr].each do |neighbor, weight|
      if !visited.includes?(neighbor)
        stack << {neighbor, dist + weight, visited}
      end
    end
  end

  longest_path
end

def pos_to_key(row, col)
  "#{row},#{col}"
end

def key_to_pos(key)
  parts = key.split(',')
  {parts[0].to_i, parts[1].to_i}
end

input_file = "input.txt"
puts "Part 1: #{solve(input_file, false)}"
puts "Part 2: #{solve(input_file, true)}"
