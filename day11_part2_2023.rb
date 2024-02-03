
Coord = Struct.new(:x, :y)

Grid = Struct.new(:width, :height, :data)

Empty = '.'

def build_grid(input, empty)
  grid = Grid.new(input[0].length, input.length, {})

  input.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      grid.data[Coord.new(x, y)] = char unless char == empty
    end
  end

  grid
end

def to_string(grid, empty)
  result = ""

  (0...grid.height).each do |y|
    (0...grid.width).each do |x|
      coord = Coord.new(x, y)
      if grid.data[coord]
        result += grid.data[coord]
      else
        result += empty
      end
    end
    result += "\n"
  end

  result
end

def get_empty_rows(grid)
  empty_rows = []

  (0...grid.height).each do |y|
    is_empty = true

    x = 0
    while x < grid.width
      unless grid.data[Coord.new(x, y)].nil?
        is_empty = false
      end
      x += 1
    end

    empty_rows << y if is_empty
  end

  empty_rows
end

def get_empty_cols(grid)
  empty_cols = []

  (0...grid.width).each do |x|
    is_empty = true

    y = 0
    while y < grid.height
      unless grid.data[Coord.new(x, y)].nil?
        is_empty = false
      end
      y += 1
    end

    empty_cols << x if is_empty
  end

  empty_cols
end

def calculate_offsets(empty_indexes, bound)
  offsets = Array.new(bound, 0)

  empty_indexes.each do |idx|
    (idx + 1...offsets.length).each do |i|
      offsets[i] += 1
    end
  end

  offsets
end

def expand_grid(grid, expansion_factor)
  empty_cols = get_empty_cols(grid)
  empty_rows = get_empty_rows(grid)
  num_lines_to_add = expansion_factor - 1

  new_grid = Grid.new(grid.width + empty_cols.length * num_lines_to_add, grid.height + empty_rows.length * num_lines_to_add, {})

  dxs = calculate_offsets(empty_cols, grid.width)
  dys = calculate_offsets(empty_rows, grid.height)

  (0...grid.height).each do |y|
    (0...grid.width).each do |x|
      coord = Coord.new(x, y)
      if grid.data[coord]
        new_coord = Coord.new(x + dxs[x] * num_lines_to_add, y + dys[y] * num_lines_to_add)
        new_grid.data[new_coord] = grid.data[coord]
      end
    end
  end

  new_grid
end

def abs(x)
  x < 0 ? -x : x
end

def calculate_length(grid, c1, c2)
  dx = abs(c2.x - c1.x)
  dy = abs(c2.y - c1.y)
  dx + dy
end

def solve(input, expansion_factor)
  grid = build_grid(input, Empty)
  expanded_grid = expand_grid(grid, expansion_factor)

  res = 0
  already_seen = {}

  expanded_grid.data.keys.each do |coord1|
    already_seen.keys.each do |coord2|
      length = calculate_length(expanded_grid, coord1, coord2)
      res += length
    end
    already_seen[coord1] = true
  end

  res
end

def read_file(file_name)
  File.readlines(file_name).map(&:chomp)
end

input = read_file("input.txt")
puts solve(input, 1000000)
