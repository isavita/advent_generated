class Coord
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def +(other)
    Coord.new(@x + other.x, @y + other.y)
  end

  def ==(other)
    @x == other.x && @y == other.y
  end

  def hash
    [@x, @y].hash
  end

  def eql?(other)
    self == other
  end
end

class Grid
  attr_accessor :width, :height, :data

  def initialize(width, height)
    @width = width
    @height = height
    @data = {}
  end
end

NORTH = Coord.new(0, -1)
SOUTH = Coord.new(0, 1)
WEST = Coord.new(-1, 0)
EAST = Coord.new(1, 0)
EMPTY = '.'
WALL = '#'
NORTH_SLOPES = '^'
SOUTH_SLOPES = 'v'
WEST_SLOPES = '<'
EAST_SLOPES = '>'
SLOPE_TO_DIR = {
  NORTH_SLOPES => NORTH,
  SOUTH_SLOPES => SOUTH,
  WEST_SLOPES => WEST,
  EAST_SLOPES => EAST
}

class Edge
  attr_accessor :start, :end, :weight

  def initialize(start, _end, weight)
    @start = start
    @end = _end
    @weight = weight
  end

  def hash
    [@start, @end, @weight].hash
  end

  def eql?(other)
    @start == other.start && @end == other.end && @weight == other.weight
  end
end

class Graph
  attr_accessor :vertices, :edges

  def initialize
    @vertices = {}
    @edges = {}
  end
end

def is_in_bounds(grid, coord)
  coord.x.between?(0, grid.width - 1) && coord.y.between?(0, grid.height - 1)
end

def parse_input(input)
  grid = Grid.new(input[0].length, input.length)

  input.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      grid.data[Coord.new(x, y)] = char unless char == EMPTY
    end
  end

  grid
end

def valid_neighbor?(grid, coord, dir)
  is_in_bounds(grid, coord) && grid.data[coord] != WALL
end

def valid_neighbor_with_slopes?(grid, coord, dir)
  is_in_bounds(grid, coord) && (!grid.data.key?(coord) || (grid.data[coord] != WALL && SLOPE_TO_DIR[grid.data[coord]] == dir))
end

def neighbors4(grid, coord, valid_neighbor_func)
  directions = [NORTH, SOUTH, WEST, EAST]
  directions.flat_map do |dir|
    neighbor = coord + dir
    send(valid_neighbor_func, grid, neighbor, dir) ? [neighbor] : []
  end
end

def get_graph(grid, start, _end, valid_neighbor_func)
  graph = Graph.new
  graph.vertices[start] = nil
  graph.vertices[_end] = nil

  (0...grid.height).each do |y|
    (0...grid.width).each do |x|
      coord = Coord.new(x, y)
      if !grid.data.key?(coord) && neighbors4(grid, coord, :valid_neighbor?).count > 2
        graph.vertices[coord] = nil
      end
    end
  end

  graph.vertices.keys.each do |start_vertex|
    graph.edges[start_vertex] = get_edges_bfs(grid, start_vertex, graph.vertices.keys, valid_neighbor_func)
  end

  graph
end

def get_edges_bfs(grid, start, vertices, valid_neighbor_func)
  frontier = [start]
  reached = { start => nil }
  distances = { start => 0 }
  edges = {}

  until frontier.empty?
    current = frontier.shift

    if vertices.include?(current) && current != start
      edge = Edge.new(start, current, distances[current])
      edges[edge] = nil
      next
    end

    neighbors4(grid, current, valid_neighbor_func).each do |next_coord|
      unless reached.key?(next_coord)
        frontier.push(next_coord)
        reached[next_coord] = nil
        distances[next_coord] = distances[current] + 1
      end
    end
  end

  edges
end

def max_distance_dfs(grid, graph, current, _end, seen)
  return [true, 0] if current == _end

  max_dist = 0
  seen[current] = nil

  graph.edges[current].keys.each do |edge|
    unless seen.key?(edge.end)
      valid, dist = max_distance_dfs(grid, graph, edge.end, _end, seen)
      max_dist = [max_dist, dist + edge.weight].max if valid
    end
  end

  seen.delete(current)

  [max_dist > 0, max_dist]
end

def solve(input)
  grid = parse_input(input)

  start = Coord.new(1, 0)
  _end = Coord.new(grid.width - 2, grid.height - 1)

  graph = get_graph(grid, start, _end, :valid_neighbor_with_slopes?)

  _, max_dist = max_distance_dfs(grid, graph, start, _end, {})
  max_dist
end

def read_file(file_name)
  File.read(file_name).strip.split("\n")
end

input = read_file("input.txt")
puts solve(input)