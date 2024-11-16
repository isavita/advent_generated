
struct Coord
  property x : Int32
  property y : Int32

  def initialize(@x, @y)
  end

  def +(other : Coord)
    Coord.new(x + other.x, y + other.y)
  end

  def *(scalar : Int32)
    Coord.new(x * scalar, y * scalar)
  end
end

class Grid
  property width : Int32
  property height : Int32
  property data : Hash(Coord, Char)

  def initialize(@width, @height, @data = {} of Coord => Char)
  end

  def to_s
    String.build do |str|
      height.times do |y|
        width.times do |x|
          coord = Coord.new(x, y)
          str << (data[coord]? || '.')
        end
        str << '\n'
      end
    end
  end
end

NORTH = Coord.new(0, -1)
WEST  = Coord.new(-1, 0)
SOUTH = Coord.new(0, 1)
EAST  = Coord.new(1, 0)

def is_in_bounds(grid : Grid, coord : Coord)
  0 <= coord.x < grid.width && 0 <= coord.y < grid.height
end

def parse_input(input : Array(String)) : Grid
  grid = Grid.new(
    width: input[0].size,
    height: input.size
  )

  input.each_with_index do |line, y|
    line.each_char_with_index do |char, x|
      grid.data[Coord.new(x, y)] = char if char != '.'
    end
  end

  grid
end

def find_start(grid : Grid) : Coord
  grid.data.each do |coord, char|
    return coord if char == 'S'
  end
  raise "No start found"
end

def neighbors4(grid : Grid, coord : Coord) : Array(Coord)
  [NORTH, SOUTH, EAST, WEST]
    .map { |dir| coord + dir }
    .select { |neighbor| is_in_bounds(grid, neighbor) && grid.data[neighbor]? != '#' }
end

def breadth_first_search(grid : Grid, start : Coord, neighbor_func : Proc(Grid, Coord, Array(Coord))) : Hash(Coord, Int32)
  frontier = [start]
  reached = Set{start}
  distances = {start => 0}

  until frontier.empty?
    current = frontier.shift

    neighbor_func.call(grid, current).each do |next_coord|
      unless reached.includes?(next_coord)
        frontier << next_coord
        reached << next_coord
        distances[next_coord] = distances[current] + 1
      end
    end
  end

  distances
end

def solve(input : Array(String), num_steps : Int32) : Int32
  grid = parse_input(input)
  start = find_start(grid)
  distances = breadth_first_search(grid, start, ->neighbors4(Grid, Coord))

  distances.count do |_, dist|
    dist <= num_steps && dist % 2 == 0
  end
end

def read_file(file_name : String) : Array(String)
  File.read_lines(file_name)
end

input = read_file("input.txt")
puts solve(input, 64)
