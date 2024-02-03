
Coord = Struct.new(:x, :y) do
  def add(c)
    Coord.new(self.x + c.x, self.y + c.y)
  end

  def substract(c)
    Coord.new(self.x - c.x, self.y - c.y)
  end

  def opposite
    Coord.new(-self.x, -self.y)
  end
end

Tile = Struct.new(:value)

Pipe = Struct.new(:coords)

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

VerticalPipe = Pipe.new([Top, Bottom])
HorizontalPipe = Pipe.new([Left, Right])
TopLeftCornerPipe = Pipe.new([Top, Left])
TopRightCornerPipe = Pipe.new([Top, Right])
BottomLeftCornerPipe = Pipe.new([Bottom, Left])
BottomRightCornerPipe = Pipe.new([Bottom, Right])

TileToPipe = {
  Vertical => VerticalPipe,
  Horizontal => HorizontalPipe,
  TopLeftCorner => TopLeftCornerPipe,
  TopRightCorner => TopRightCornerPipe,
  BottomLeftCorner => BottomLeftCornerPipe,
  BottomRightCorner => BottomRightCornerPipe
}

def get_pipe_from_tile(tile)
  TileToPipe[tile] || Pipe.new([])
end

def get_tile_from_pipe(pipe)
  TileToPipe.key(pipe) || Empty
end

def build_grid(input)
  width = input[0].length
  height = input.length
  data = {}

  input.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      data[Coord.new(x, y)] = Tile.new(char) unless char == Empty.value
    end
  end

  Grid.new(width, height, data)
end

def find_start(grid)
  grid.data.key(Start)
end

def get_pipe_from_neighbors(coord, grid)
  pipe = []

  possible_neighbors = {
    Top => coord.add(Top),
    Right => coord.add(Right),
    Bottom => coord.add(Bottom),
    Left => coord.add(Left)
  }

  possible_neighbors.each do |dir, neighbor_coord|
    neighbor_pipe = get_pipe_from_tile(grid.data[neighbor_coord])
    pipe << dir if neighbor_pipe.coords.include?(dir.opposite)
  end

  Pipe.new(pipe)
end

def path_finding(start, grid)
  path = [start]
  start_pipe = get_pipe_from_neighbors(start, grid).coords

  previous_dir = nil
  current = nil

  start_pipe.each do |dir|
    previous_dir = dir
    current = start.add(dir)
  end

  until current == start
    path << current
    current_pipe = get_pipe_from_tile(grid.data[current]).coords
    current_pipe.each do |dir|
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
  new_data = {}

  path.each do |coord|
    new_data[coord] = grid.data[coord]
  end

  start = path[0]
  new_data[start] = get_tile_from_pipe(get_pipe_from_neighbors(start, grid))

  Grid.new(grid.width, grid.height, new_data)
end

def inside?(coord, grid, empty)
  return false if grid.data.key?(coord)

  start_pipe = empty
  num_pipe_on_left = 0

  coord.x.times do |x|
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
  path_grid = get_path_grid(grid, path, Empty)

  count = 0

  grid.height.times do |y|
    grid.width.times do |x|
      coord = Coord.new(x, y)
      count += 1 if inside?(coord, path_grid, Empty)
    end
  end

  count
end

input = File.readlines('input.txt', chomp: true)
puts solve(input)
