
Coord = Struct.new(:x, :y) do
  def add(other)
    Coord.new(x + other.x, y + other.y)
  end
end

class Grid
  attr_reader :width, :height, :data

  def initialize(width, height, data)
    @width = width
    @height = height
    @data = data
  end

  def to_s
    res = ""
    (0...height).each do |y|
      (0...width).each do |x|
        if data[Coord.new(x, y)]
          res += data[Coord.new(x, y)]
        else
          res += "."
        end
      end
      res += "\n"
    end
    res
  end
end

North = Coord.new(0, -1)
West = Coord.new(-1, 0)
South = Coord.new(0, 1)
East = Coord.new(1, 0)

Empty = "."
Rock = "#"
Start = "S"

def is_in_bounds(grid, coord)
  coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height
end

def parse_input(input)
  data = {}
  input.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      data[Coord.new(x, y)] = char if char != Empty
    end
  end
  Grid.new(input[0].length, input.length, data)
end

def find_start(grid)
  grid.data.each do |coord, char|
    return coord if char == Start
  end
  raise "No start found."
end

def neighbors4(grid, coord)
  neighbors = [
    coord.add(North),
    coord.add(South),
    coord.add(East),
    coord.add(West)
  ]

  valid_neighbors = neighbors.select { |neighbor| is_in_bounds(grid, neighbor) && grid.data[neighbor] != Rock }
end

def breadth_first_search(grid, start, neighbor_func)
  frontier = [start]
  reached = { start => true }
  came_from = { start => start }
  distances = { start => 0 }

  until frontier.empty?
    current = frontier.shift

    neighbor_func.call(grid, current).each do |next_coord|
      unless reached[next_coord]
        frontier << next_coord
        reached[next_coord] = true
        came_from[next_coord] = current
        distances[next_coord] = distances[current] + 1
      end
    end
  end

  distances
end

def distances_from_extremities(grid)
  distances = {}

  extremities = [
    Coord.new(0, 0),
    Coord.new(grid.width / 2, 0),
    Coord.new(grid.width, 0),
    Coord.new(grid.width, grid.height / 2),
    Coord.new(grid.width, grid.height),
    Coord.new(grid.width / 2, grid.height),
    Coord.new(0, grid.height),
    Coord.new(0, grid.height / 2)
  ]

  extremities.each do |start|
    distances[start] = breadth_first_search(grid, start, method(:neighbors4))
  end

  distances
end

def solve(input, num_steps)
  grid = parse_input(input)
  start = find_start(grid)
  distances = breadth_first_search(grid, start, method(:neighbors4))

  cnt = 0
  distances.each do |_, dist|
    cnt += 1 if dist <= num_steps && dist.even?
  end
  cnt
end

def read_file(file_name)
  File.readlines(file_name).map(&:chomp)
end

input = read_file("input.txt")
puts solve(input, 64)
