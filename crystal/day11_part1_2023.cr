
struct Grid
  property width : Int32
  property height : Int32
  property data : Hash(Tuple(Int32, Int32), Char)

  def initialize(width : Int32, height : Int32, data : Hash(Tuple(Int32, Int32), Char))
    @width = width
    @height = height
    @data = data
  end
end

def build_grid(input_lines : Array(String)) : Grid
  height = input_lines.size
  width = input_lines[0].size
  data = Hash(Tuple(Int32, Int32), Char).new

  input_lines.each_with_index do |line, y|
    line.each_char.each_with_index do |char, x|
      unless char == '.'
        data[{x.to_i32, y.to_i32}] = char
      end
    end
  end

  Grid.new(width, height, data)
end

def get_empty_rows(grid : Grid) : Array(Int32)
  empty_rows = [] of Int32

  (0...grid.height).each do |y|
    is_empty = true

    (0...grid.width).each do |x|
      if grid.data.has_key?({x.to_i32, y.to_i32})
        is_empty = false
        break
      end
    end

    if is_empty
      empty_rows << y
    end
  end

  empty_rows
end

def get_empty_cols(grid : Grid) : Array(Int32)
  empty_cols = [] of Int32

  (0...grid.width).each do |x|
    is_empty = true

    (0...grid.height).each do |y|
      if grid.data.has_key?({x.to_i32, y.to_i32})
        is_empty = false
        break
      end
    end

    if is_empty
      empty_cols << x
    end
  end

  empty_cols
end

def calculate_offsets(empty_indexes : Array(Int32), bound : Int32) : Array(Int32)
  offsets = Array.new(bound, 0)

  empty_indexes.each do |idx|
    (idx + 1).upto(offsets.size - 1) do |i|
      offsets[i] += 1
    end
  end

  offsets
end

def expand_grid(grid : Grid, expansion_factor : Int32) : Grid
  empty_cols = get_empty_cols(grid)
  empty_rows = get_empty_rows(grid)
  num_lines_to_add = expansion_factor - 1

  new_width = grid.width + empty_cols.size * num_lines_to_add
  new_height = grid.height + empty_rows.size * num_lines_to_add
  new_data = Hash(Tuple(Int32, Int32), Char).new

  d_xs = calculate_offsets(empty_cols, grid.width)
  d_ys = calculate_offsets(empty_rows, grid.height)

  grid.data.each do |(x, y), value|
    new_x = x + d_xs[x] * num_lines_to_add
    new_y = y + d_ys[y] * num_lines_to_add
    new_data[{new_x, new_y}] = value
  end

  Grid.new(new_width, new_height, new_data)
end

def calculate_length(c1 : Tuple(Int32, Int32), c2 : Tuple(Int32, Int32)) : Int32
  dx = (c2[0] - c1[0]).abs
  dy = (c2[1] - c1[1]).abs
  return dx + dy
end

def solve(input_lines : Array(String)) : Int64
  grid = build_grid(input_lines)
  expanded_grid = expand_grid(grid, 2)

  res : Int64 = 0
  already_seen = Set(Tuple(Int32, Int32)).new

  expanded_grid.data.keys.each do |coord1|
    already_seen.each do |coord2|
      length = calculate_length(coord1, coord2)
      res += length
    end
    already_seen.add(coord1)
  end

  res
end

def read_file(file_name : String) : Array(String)
  File.read_lines(file_name).map &.strip
end

def main
  input_lines = read_file("input.txt")
  puts solve(input_lines)
end

main
