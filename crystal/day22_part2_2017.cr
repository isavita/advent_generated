require "file_utils"

class Position
  getter x, y

  def initialize(@x : Int32, @y : Int32)
  end

  def hash
    x.hash ^ y.hash
  end

  def ==(other : Position)
    x == other.x && y == other.y
  end
end

enum Status
  Clean
  Weakened
  Infected
  Flagged
end

dx = [0, 1, 0, -1]
dy = [-1, 0, 1, 0]

grid = Hash(Position, Status).new(Status::Clean)
start_x, start_y = 0, 0

File.read_lines("input.txt").each_with_index do |line, y|
  line.each_char_with_index do |c, x|
    if c == '#'
      grid[Position.new(x, y)] = Status::Infected
    end
  end
  start_x, start_y = line.size // 2, y // 2
end

x, y, dir = start_x, start_y, 0
infected_count = 0

10000000.times do
  pos = Position.new(x, y)
  case grid[pos]?
  when Status::Clean
    dir = (dir - 1) % 4
    grid[pos] = Status::Weakened
  when Status::Weakened
    grid[pos] = Status::Infected
    infected_count += 1
  when Status::Infected
    dir = (dir + 1) % 4
    grid[pos] = Status::Flagged
  when Status::Flagged
    dir = (dir + 2) % 4
    grid[pos] = Status::Clean
  else
    dir = (dir - 1) % 4
    grid[pos] = Status::Weakened
  end
  x += dx[dir]
  y += dy[dir]
end

puts infected_count