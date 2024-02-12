
def read_first_row(filename : String) : String
  File.read(filename).lines.first
end

def count_safe_tiles(first_row : String, total_rows : Int) : Int32
  current_row = first_row
  safe_count = current_row.count(".")

  (1...total_rows).each do |i|
    next_row = ""
    current_row.each_char.with_index do |char, j|
      if is_trap(j-1, j, j+1, current_row)
        next_row += "^"
      else
        next_row += "."
        safe_count += 1
      end
    end
    current_row = next_row
  end

  safe_count
end

def is_trap(left : Int32, center : Int32, right : Int32, row : String) : Bool
  l = safe_if_out_of_bounds(left, row)
  c = row[center]
  r = safe_if_out_of_bounds(right, row)

  (l == '^' && c == '^' && r == '.') ||
  (c == '^' && r == '^' && l == '.') ||
  (l == '^' && c == '.' && r == '.') ||
  (r == '^' && c == '.' && l == '.')
end

def safe_if_out_of_bounds(index : Int32, row : String) : Char
  if index < 0 || index >= row.size
    '.'
  else
    row[index]
  end
end

def count_char(str : String, char : Char) : Int32
  str.count(char)
end

first_row = read_first_row("input.txt")
total_rows = 40
safe_tiles_count = count_safe_tiles(first_row, total_rows)
puts safe_tiles_count
