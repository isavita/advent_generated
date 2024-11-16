
def solve(input : String) : Int32
  matrix = parse_input(input)
  origin_col = 0
  matrix[0].each_with_index do |c, i|
    origin_col = i if c == "+"
    matrix[-1][i] = "#"
  end

  ans = 0
  loop do
    break if drop_sand(matrix, origin_col)
    ans += 1
    break if matrix[0][origin_col] == "o"
  end

  ans
end

def parse_input(input : String)
  coord_sets = [] of Array(Tuple(Int32, Int32))
  lowest_col = Int32::MAX
  highest_row = 0

  input.split('\n').each do |line|
    coords = line.split(" -> ").map do |raw_coord|
      nums = raw_coord.split(',').map(&.to_i)
      col, row = nums[0], nums[1]
      lowest_col = Math.min(lowest_col, col)
      highest_row = Math.max(highest_row, row)
      {col, row}
    end
    coord_sets << coords
  end

  extra_left_space = 200
  highest_col = 0

  coord_sets = coord_sets.map do |set|
    set.map do |coord|
      {coord[0] - (lowest_col - extra_left_space), coord[1]}.tap do |new_coord|
        highest_col = Math.max(highest_col, new_coord[0])
      end
    end
  end

  matrix = Array.new(highest_row + 3) { Array.new(highest_col + extra_left_space * 2, ".") }

  coord_sets.each do |set|
    set.each_cons(2) do |pair|
      start, finish = pair
      if start[0] == finish[0]
        (Math.min(start[1], finish[1])..Math.max(start[1], finish[1])).each do |r|
          matrix[r][start[0]] = "#"
        end
      elsif start[1] == finish[1]
        (Math.min(start[0], finish[0])..Math.max(start[0], finish[0])).each do |c|
          matrix[start[1]][c] = "#"
        end
      end
    end
  end

  origin_col = 500 - lowest_col + extra_left_space
  matrix[0][origin_col] = "+"

  matrix
end

def drop_sand(matrix : Array(Array(String)), origin_col : Int32) : Bool
  r, c = 0, origin_col

  while r < matrix.size - 1
    below = matrix[r + 1][c]
    diagonally_left = matrix[r + 1][c - 1]
    diagonally_right = matrix[r + 1][c + 1]

    if below == "."
      r += 1
    elsif diagonally_left == "."
      r += 1
      c -= 1
    elsif diagonally_right == "."
      r += 1
      c += 1
    else
      matrix[r][c] = "o"
      return false
    end
  end

  true
end

input = File.read("input.txt").strip
puts solve(input)
