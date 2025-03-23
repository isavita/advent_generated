
struct Coord
  property x : Int64
  property y : Int64

  def initialize(@x : Int64, @y : Int64)
  end

  def +(other : Coord) : Coord
    Coord.new(x + other.x, y + other.y)
  end

  def *(scalar : Int64) : Coord
    Coord.new(x * scalar, y * scalar)
  end
end

def parse_input(input : Array(String)) : Array(Coord)
  north = Coord.new(0, -1)
  west = Coord.new(-1, 0)
  south = Coord.new(0, 1)
  east = Coord.new(1, 0)

  current = Coord.new(0, 0)
  vertices = [current]

  input.each do |line|
    parts = line.split(" ")
    dir_input = parts[0][0]
    length = parts[1].to_i64

    dir = case dir_input
          when 'U'
            north
          when 'L'
            west
          when 'D'
            south
          when 'R'
            east
          else
            raise "Invalid direction"
          end

    current = current + (dir * length)
    vertices << current
  end

  vertices
end

def shoelace(vertices : Array(Coord)) : Int64
  n = vertices.size
  area = 0_i64

  (0...n).each do |i|
    next_index = (i + 1) % n
    area += vertices[i].x * vertices[next_index].y
    area -= vertices[i].y * vertices[next_index].x
  end

  area.abs // 2
end

def perimeter(vertices : Array(Coord)) : Int64
  n = vertices.size
  perim = 0_i64

  (0...n).each do |i|
    next_index = (i + 1) % n
    perim += (vertices[i].x - vertices[next_index].x).abs + (vertices[i].y - vertices[next_index].y).abs
  end

  perim
end

def calculate_polygon_area(vertices : Array(Coord)) : Int64
  shoelace(vertices) + perimeter(vertices) // 2 + 1
end

def solve(input : Array(String)) : Int64
  vertices = parse_input(input)
  calculate_polygon_area(vertices)
end

def main
  input = File.read_lines("input.txt")
  puts solve(input)
end

main
