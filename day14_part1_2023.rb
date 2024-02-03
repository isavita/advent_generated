
Coord = Struct.new(:x, :y) do
  def add(other)
    Coord.new(x + other.x, y + other.y)
  end

  def in_bounds?(grid)
    x >= 0 && x < grid[:width] && y >= 0 && y < grid[:height]
  end
end

Grid = Struct.new(:width, :height, :data)

def build_grid(input)
  grid = Grid.new(input[0].length, input.length, {})

  input.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      grid[:data][Coord.new(x, y)] = char unless char == '.'
    end
  end

  grid
end

def to_string(grid)
  result = ""

  (0...grid[:height]).each do |y|
    (0...grid[:width]).each do |x|
      coord = Coord.new(x, y)
      if grid[:data][coord]
        result += grid[:data][coord]
      else
        result += '.'
      end
    end
    result += "\n"
  end

  result
end

def shift_single_rock(grid, coord, dir)
  if grid[:data][coord] == 'O'
    current = coord
    before = current.add(dir)

    until grid[:data][before] || !before.in_bounds?(grid)
      grid[:data][before] = 'O'
      grid[:data].delete(current)

      current = before
      before = before.add(dir)
    end
  end
end

def shift_rocks(grid, dir)
  case dir
  when Coord.new(0, -1), Coord.new(-1, 0)
    (0...grid[:width]).each do |x|
      (0...grid[:height]).each do |y|
        shift_single_rock(grid, Coord.new(x, y), dir)
      end
    end

  when Coord.new(0, 1), Coord.new(1, 0)
    (grid[:width]-1).downto(0) do |x|
      (grid[:height]-1).downto(0) do |y|
        shift_single_rock(grid, Coord.new(x, y), dir)
      end
    end
  end
end

def calculate_load(grid)
  load = 0

  (0...grid[:width]).each do |x|
    (0...grid[:height]).each do |y|
      coord = Coord.new(x, y)
      load += grid[:height] - y if grid[:data][coord] == 'O'
    end
  end

  load
end

def solve(input)
  grid = build_grid(input)
  shift_rocks(grid, Coord.new(0, -1))

  calculate_load(grid)
end

input = File.readlines("input.txt", chomp: true)
puts solve(input)
