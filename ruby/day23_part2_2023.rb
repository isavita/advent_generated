
Coord = Struct.new(:x, :y) do
  def add(other)
    Coord.new(x + other.x, y + other.y)
  end
end

NORTH = Coord.new(0, -1)
SOUTH = Coord.new(0, 1)
WEST = Coord.new(-1, 0)
EAST = Coord.new(1, 0)

SLOPE_TO_DIR = {
  '^' => NORTH,
  'v' => SOUTH,
  '<' => WEST,
  '>' => EAST
}.freeze

Edge = Struct.new(:start, :end, :weight)

def in_bounds?(grid, coord)
  coord.x >= 0 && coord.x < grid.first.size && coord.y >= 0 && coord.y < grid.size
end

def parse_input(input)
  grid = input.map { |line| line.chars }
  grid
end

def valid_neighbor?(grid, coord, _dir)
  return false unless in_bounds?(grid, coord)
  return false if grid[coord.y][coord.x] == '#'

  true
end

def neighbors4(grid, coord)
  directions = [NORTH, SOUTH, WEST, EAST]
  valid_neighbors = []

  directions.each do |dir|
    neighbor = coord.add(dir)
    valid_neighbors << neighbor if valid_neighbor?(grid, neighbor, dir)
  end

  valid_neighbors
end

def get_graph(grid, start, end_coord)
  vertices = { start => true, end_coord => true }
  edges = Hash.new { |h, k| h[k] = [] }

  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      coord = Coord.new(x, y)
      next if cell != '.'

      vertices[coord] = true if neighbors4(grid, coord).size > 2
    end
  end

  vertices.keys.each do |start_vertex|
    edges[start_vertex] = get_edges_bfs(grid, start_vertex, vertices)
  end

  [vertices, edges]
end

def get_edges_bfs(grid, start, vertices)
  frontier = [start]
  reached = { start => true }
  distances = { start => 0 }
  edges = []

  until frontier.empty?
    current = frontier.shift

    if vertices[current] && current != start
      edges << Edge.new(start, current, distances[current])
      next
    end

    neighbors4(grid, current).each do |next_coord|
      next if reached[next_coord]

      frontier << next_coord
      reached[next_coord] = true
      distances[next_coord] = distances[current] + 1
    end
  end

  edges
end

def get_max_distance_dfs(edges, current, end_coord, seen)
  return [true, 0] if current == end_coord

  maxi = 0
  seen[current] = true
  edges[current].each do |edge|
    next if seen[edge.end]

    is_valid, dist = get_max_distance_dfs(edges, edge.end, end_coord, seen)
    maxi = [maxi, dist + edge.weight].max if is_valid
  end
  seen.delete(current)

  return [false, 0] if maxi.zero?

  [true, maxi]
end

def solve(input)
  grid = parse_input(input)
  start = Coord.new(1, 0)
  end_coord = Coord.new(grid.first.size - 2, grid.size - 1)

  vertices, edges = get_graph(grid, start, end_coord)

  _, max_dist = get_max_distance_dfs(edges, start, end_coord, {})
  max_dist
end

input = File.readlines('input.txt').map(&:chomp)
puts solve(input)
