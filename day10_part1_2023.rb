
Coord = Struct.new(:x, :y) do
  def add(c2)
    Coord.new(x + c2.x, y + c2.y)
  end

  def substract(c2)
    Coord.new(x - c2.x, y - c2.y)
  end

  def opposite
    Coord.new(-x, -y)
  end
end

Tile = Struct.new(:value)

Pipe = Hash.new

Grid = Struct.new(:width, :height, :data)

Undefined = Coord.new(0, 0)
Top = Coord.new(0, -1)
Right = Coord.new(1, 0)
Bottom = Coord.new(0, 1)
Left = Coord.new(-1, 0)

Empty = Tile.new('.')
Start = Tile.new('S')
Vertical = Tile.new('|')
Horizontal = Tile.new('-')
TopLeftCorner = Tile.new('J')
TopRightCorner = Tile.new('L')
BottomLeftCorner = Tile.new('7')
BottomRightCorner = Tile.new('F')
Enclosed = Tile.new('X')

VerticalPipe = { Top => nil, Bottom => nil }
HorizontalPipe = { Left => nil, Right => nil }
TopLeftCornerPipe = { Top => nil, Left => nil }
TopRightCornerPipe = { Top => nil, Right => nil }
BottomLeftCornerPipe = { Bottom => nil, Left => nil }
BottomRightCornerPipe = { Bottom => nil, Right => nil }

TileToPipe = {
  Vertical => VerticalPipe,
  Horizontal => HorizontalPipe,
  TopLeftCorner => TopLeftCornerPipe,
  TopRightCorner => TopRightCornerPipe,
  BottomLeftCorner => BottomLeftCornerPipe,
  BottomRightCorner => BottomRightCornerPipe
}

def get_pipe_from_tile(tile)
  TileToPipe[tile] || {}
end

def get_tile_from_pipe(pipe)
  TileToPipe.key(pipe) || Empty
end

class Hash
  def equal_pipe?(other)
    size == other.size && all? { |k, v| other.key?(k) }
  end
end

def build_grid(input)
  width = input[0].length
  height = input.length
  data = {}

  input.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      data[Coord.new(x, y)] = Tile.new(char) unless char == '.'
    end
  end

  Grid.new(width, height, data)
end

def to_string(grid)
  pipes_representations = {
    Empty => ' ',
    Start => 'S',
    Vertical => '║',
    Horizontal => '═',
    TopLeftCorner => '╝',
    TopRightCorner => '╚',
    BottomLeftCorner => '╗',
    BottomRightCorner => '╔',
    Enclosed => 'X'
  }

  res = ""

  (0...grid.height).each do |y|
    (0...grid.width).each do |x|
      coord = Coord.new(x, y)
      res += pipes_representations[grid.data[coord]] || pipes_representations[Empty]
    end
    res += "\n"
  end

  res
end

def find_start(grid)
  grid.data.key(Start)
end

def get_pipe_from_neighbors(coord, grid)
  pipe = {}

  possible_neighbors = {
    Top => coord.add(Top),
    Right => coord.add(Right),
    Bottom => coord.add(Bottom),
    Left => coord.add(Left)
  }

  possible_neighbors.each do |dir, neighbor_coord|
    neighbor_pipe = get_pipe_from_tile(grid.data[neighbor_coord])
    pipe[dir] = nil if neighbor_pipe.key?(dir.opposite)
  end

  pipe
end

def path_finding(start, grid)
  path = [start]
  start_pipe = get_pipe_from_neighbors(start, grid)

  previous_dir = nil
  current = nil
  start_pipe.each_key do |dir|
    previous_dir = dir
    current = start.add(dir)
  end

  until current == start
    path << current
    current_pipe = get_pipe_from_tile(grid.data[current])
    current_pipe.each_key do |dir|
      if dir != previous_dir.opposite
        previous_dir = dir
        current = current.add(dir)
        break
      end
    end
  end

  path
end

def get_path_grid(grid, path, empty)
  new_grid = Grid.new(grid.width, grid.height, {})

  path.each do |coord|
    new_grid.data[coord] = grid.data[coord]
  end

  start = path[0]
  new_grid.data[start] = get_tile_from_pipe(get_pipe_from_neighbors(start, grid))

  new_grid
end

def inside?(coord, grid, empty)
  return false if grid.data.key?(coord)

  start_pipe = empty
  num_pipe_on_left = 0

  (0...coord.x).each do |x|
    coord = Coord.new(x, coord.y)
    v = grid.data[coord]

    case v
    when Vertical
      num_pipe_on_left += 1
    when TopRightCorner
      start_pipe = TopRightCorner
    when BottomRightCorner
      start_pipe = BottomRightCorner
    when TopLeftCorner
      if start_pipe == BottomRightCorner
        start_pipe = empty
        num_pipe_on_left += 1
      elsif v == TopRightCorner
        start_pipe = Empty
      end
    when BottomLeftCorner
      if start_pipe == TopRightCorner
        start_pipe = Empty
        num_pipe_on_left += 1
      elsif start_pipe == BottomRightCorner
        start_pipe = Empty
      end
    end
  end

  num_pipe_on_left.odd?
end

def solve(input)
  grid = build_grid(input)
  start = find_start(grid)
  path = path_finding(start, grid)
  num_pipes_visited = path.length
  max_length = num_pipes_visited / 2
  max_length
end

def read_file(file_name)
  File.read(file_name).split("\n")
end

input = read_file("input.txt")
puts solve(input)
