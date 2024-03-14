class Coord
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def add(other)
    Coord.new(@x + other.x, @y + other.y)
  end

  def multiply_by_scalar(scalar)
    Coord.new(@x * scalar, @y * scalar)
  end
end

NORTH = Coord.new(0, -1)
WEST = Coord.new(-1, 0)
SOUTH = Coord.new(0, 1)
EAST = Coord.new(1, 0)

def abs(x)
  x < 0 ? -x : x
end

def parse_input(input)
  current = Coord.new(0, 0)
  vertices = [current]

  input.each do |line|
    dir, length = line.split
    case dir
    when 'U'
      dir_coord = NORTH
    when 'L'
      dir_coord = WEST
    when 'D'
      dir_coord = SOUTH
    when 'R'
      dir_coord = EAST
    end

    length = length.to_i
    current = current.add(dir_coord.multiply_by_scalar(length))
    vertices << current
  end

  vertices
end

def shoelace(vertices)
  n = vertices.length
  area = 0

  (0...n).each do |i|
    next_i = (i + 1) % n
    area += vertices[i].x * vertices[next_i].y
    area -= vertices[i].y * vertices[next_i].x
  end

  (area.abs / 2).to_i
end

def perimeter(vertices)
  n = vertices.length
  perim = 0

  (0...n).each do |i|
    next_i = (i + 1) % n
    perim += abs(vertices[i].x - vertices[next_i].x) + abs(vertices[i].y - vertices[next_i].y)
  end

  perim
end

def calculate_polygon_area(vertices)
  shoelace(vertices) + perimeter(vertices) / 2 + 1
end

def solve(input)
  vertices = parse_input(input)
  calculate_polygon_area(vertices)
end

def read_file(file_name)
  File.readlines(file_name, chomp: true)
end

input = read_file('input.txt')
puts solve(input)