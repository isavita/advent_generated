
require "set"

struct Coord
  getter x : Int32
  getter y : Int32
  def initialize(@x : Int32, @y : Int32); end
  def +(other : Coord) : Coord
    Coord.new(x + other.x, y + other.y)
  end
  def -(other : Coord) : Coord
    Coord.new(x - other.x, y - other.y)
  end
  def opposite : Coord
    Coord.new(-x, -y)
  end
end

Empty = '.' ; Start = 'S' ; Vertical = '|' ; Horizontal = '-' ;
TopLeftCorner = 'J' ; TopRightCorner = 'L' ; BottomLeftCorner = '7' ; BottomRightCorner = 'F'

Top = Coord.new(0, -1) ; Right = Coord.new(1, 0) ; Bottom = Coord.new(0, 1) ; Left = Coord.new(-1, 0)

VerticalPipe   = Set{Top, Bottom}
HorizontalPipe = Set{Left, Right}
TopLeftPipe    = Set{Top, Left}
TopRightPipe   = Set{Top, Right}
BottomLeftPipe = Set{Bottom, Left}
BottomRightPipe= Set{Bottom, Right}

TileToPipe = {
  Vertical          => VerticalPipe,
  Horizontal        => HorizontalPipe,
  TopLeftCorner     => TopLeftPipe,
  TopRightCorner    => TopRightPipe,
  BottomLeftCorner  => BottomLeftPipe,
  BottomRightCorner => BottomRightPipe
}

def get_pipe_from_tile(tile : Char) : Set(Coord)
  TileToPipe[tile]? || Set(Coord).new
end

def get_tile_from_pipe(pipe : Set(Coord)) : Char
  TileToPipe.each do |t, p|
    return t if p == pipe
  end
  Empty
end

def build_grid(lines : Array(String)) : Hash(Coord, Char)
  grid = Hash(Coord, Char).new
  lines.each_with_index do |line, y|
    line.each_char.with_index do |ch, x|
      grid[Coord.new(x, y)] = ch if ch != Empty
    end
  end
  grid
end

def find_start(grid : Hash(Coord, Char)) : Coord
  grid.each do |c, t|
    return c if t == Start
  end
  Coord.new(0, 0)
end

def get_pipe_from_neighbors(coord : Coord, grid : Hash(Coord, Char)) : Set(Coord)
  pipe = Set(Coord).new
  {Top => coord + Top, Right => coord + Right, Bottom => coord + Bottom, Left => coord + Left}.each do |dir, n|
    if (tile = grid[n]?) && get_pipe_from_tile(tile).includes?(dir.opposite)
      pipe << dir
    end
  end
  pipe
end

def path_finding(start : Coord, grid : Hash(Coord, Char)) : Array(Coord)
  path = [start]
  start_pipe = get_pipe_from_neighbors(start, grid)
  previous_dir = start_pipe.first
  current = start + previous_dir
  while current != start
    path << current
    cur_pipe = get_pipe_from_tile(grid[current])
    cur_pipe.each do |dir|
      if dir != previous_dir.opposite
        previous_dir = dir
        current = current + dir
        break
      end
    end
  end
  path
end

def get_path_grid(grid : Hash(Coord, Char), path : Array(Coord)) : Hash(Coord, Char)
  new_grid = Hash(Coord, Char).new
  path.each { |c| new_grid[c] = grid[c] }
  start = path.first
  new_grid[start] = get_tile_from_pipe(get_pipe_from_neighbors(start, grid))
  new_grid
end

def is_inside(coord : Coord, grid : Hash(Coord, Char)) : Bool
  return false if grid.has_key?(coord)
  start_pipe = Empty
  left_cross = 0
  (0...coord.x).each do |x|
    c = Coord.new(x, coord.y)
    next unless (v = grid[c]?)
    case v
    when Vertical
      left_cross += 1
    when TopRightCorner
      start_pipe = TopRightCorner
    when BottomRightCorner
      start_pipe = BottomRightCorner
    when TopLeftCorner
      if start_pipe == BottomRightCorner
        start_pipe = Empty
        left_cross += 1
      elsif start_pipe == TopRightCorner
        start_pipe = Empty
      end
    when BottomLeftCorner
      if start_pipe == TopRightCorner
        start_pipe = Empty
        left_cross += 1
      elsif start_pipe == BottomRightCorner
        start_pipe = Empty
      end
    end
  end
  left_cross.odd?
end

def solve(lines : Array(String)) : Int32
  grid = build_grid(lines)
  start = find_start(grid)
  path = path_finding(start, grid)
  path_grid = get_path_grid(grid, path)
  count = 0
  lines.each_with_index do |line, y|
    line.size.times do |x|
      count += 1 if is_inside(Coord.new(x, y), path_grid)
    end
  end
  count
end

input = File.read_lines("input.txt").map(&.strip)
puts solve(input)
