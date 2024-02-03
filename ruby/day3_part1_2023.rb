
def read_file_to_matrix(file_path)
  matrix = []
  File.open(file_path, "r") do |file|
    file.each_line do |line|
      matrix << line.chomp.chars
    end
  end
  matrix
end

def sum_of_part_numbers(matrix)
  sum = 0
  visited = Array.new(matrix.length) { Array.new(matrix[0].length, false) }

  matrix.each_with_index do |row, y|
    row.each_with_index do |_, x|
      if !visited[y][x] && matrix[y][x].match?(/\d/)
        number, length = extract_number(matrix, x, y)
        if is_adjacent_to_symbol(matrix, x, y, length)
          sum += number
        end
        length.times { |i| visited[y][x + i] = true }
      end
    end
  end

  sum
end

def extract_number(matrix, x, y)
  number_str = ""
  while x < matrix[y].length && matrix[y][x].match?(/\d/)
    number_str += matrix[y][x]
    x += 1
  end
  [number_str.to_i, number_str.length]
end

def is_adjacent_to_symbol(matrix, x, y, length)
  length.times do |i|
    return true if check_adjacent(matrix, x + i, y)
  end
  false
end

def check_adjacent(matrix, x, y)
  (-1..1).each do |dy|
    (-1..1).each do |dx|
      adj_x, adj_y = x + dx, y + dy
      if adj_y >= 0 && adj_y < matrix.length && adj_x >= 0 && adj_x < matrix[adj_y].length
        return true if !matrix[adj_y][adj_x].match?(/\d/) && matrix[adj_y][adj_x] != '.'
      end
    end
  end
  false
end

matrix = read_file_to_matrix("input.txt")
sum = sum_of_part_numbers(matrix)
puts sum
