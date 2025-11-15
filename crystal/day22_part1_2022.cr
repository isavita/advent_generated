
# Crystal port of the C solution for the monkey-map path tracing task.
# Reads from input.txt, prints the final password.

class Boundary
  property min : Int32
  property max : Int32

  def initialize
    @min = 0
    @max = 0
  end
end

class Position
  property x : Int32
  property y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

lines = File.read_lines("input.txt")
blank = lines.index("")
abort "Bad input" unless blank

grid_lines = lines[0, blank]
path = lines[blank + 1]

rows = grid_lines.size
cols = grid_lines.max_of &.size

grid = Array.new(rows) { |y| Array.new(cols, ' ') }
grid_lines.each_with_index do |line, y|
  line.chars.each_with_index { |ch, x| grid[y][x] = ch }
end

row_bounds = Array.new(rows) { Boundary.new }
col_bounds = Array.new(cols) { Boundary.new }

rows.times do |y|
  cols.times do |x|
    next if grid[y][x] == ' '
    row_bounds[y].min = x + 1 if row_bounds[y].min == 0
    row_bounds[y].max = x + 1
  end
end

cols.times do |x|
  rows.times do |y|
    next if grid[y][x] == ' '
    col_bounds[x].min = y + 1 if col_bounds[x].min == 0
    col_bounds[x].max = y + 1
  end
end

start_x = grid[0].index('.').not_nil! + 1
pos = Position.new(start_x, 1)
facing = 0 # R D L U

dx = {1, 0, -1, 0}
dy = {0, 1, 0, -1}

i = 0
while i < path.size
  if path[i].ascii_number?
    steps = 0
    while i < path.size && path[i].ascii_number?
      steps = steps * 10 + path[i].to_i
      i += 1
    end

    steps.times do
      nx = pos.x + dx[facing]
      ny = pos.y + dy[facing]

      if facing == 0 && nx > row_bounds[pos.y - 1].max
        nx = row_bounds[pos.y - 1].min
      elsif facing == 2 && nx < row_bounds[pos.y - 1].min
        nx = row_bounds[pos.y - 1].max
      elsif facing == 1 && ny > col_bounds[pos.x - 1].max
        ny = col_bounds[pos.x - 1].min
      elsif facing == 3 && ny < col_bounds[pos.x - 1].min
        ny = col_bounds[pos.x - 1].max
      end

      break if grid[ny - 1][nx - 1] == '#'
      if grid[ny - 1][nx - 1] == '.'
        pos.x = nx
        pos.y = ny
      end
    end
  elsif path[i] == 'R'
    facing = (facing + 1) % 4
    i += 1
  elsif path[i] == 'L'
    facing = (facing + 3) % 4
    i += 1
  else
    i += 1
  end
end

puts 1000_i64 * pos.y + 4_i64 * pos.x + facing
