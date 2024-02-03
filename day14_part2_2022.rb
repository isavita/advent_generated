
file = File.read("input.txt")

def solve(input)
  matrix = parse_input(input)
  origin_col = 0
  matrix[0].each_with_index do |c, i|
    origin_col = i if c == "+"
    matrix[matrix.length - 1][i] = "#"
  end

  ans = 0
  until drop_sand(matrix, origin_col)
    ans += 1
    break if matrix[0][origin_col] == "o"
  end

  ans
end

def parse_input(input)
  coord_sets = []
  lowest_col = Float::INFINITY
  highest_row = 0

  input.split("\n").each do |line|
    raw_coords = line.split(" -> ")
    coords = []

    raw_coords.each do |raw_coord|
      raw_nums = raw_coord.split(",")
      col = raw_nums[0].to_i
      row = raw_nums[1].to_i
      coord = [col, row]
      coords << coord

      lowest_col = [lowest_col, col].min
      highest_row = [highest_row, row].max
    end

    coord_sets << coords
  end

  extra_left_space = 200
  highest_col = 0

  coord_sets.each do |set|
    set.each do |coord|
      coord[0] -= lowest_col - extra_left_space
      highest_col = [highest_col, coord[0]].max
    end
  end

  matrix = Array.new(highest_row + 3) { Array.new(highest_col + extra_left_space * 2, ".") }

  coord_sets.each do |set|
    (1..set.length - 1).each do |i|
      cols = [set[i - 1][0], set[i][0]]
      rows = [set[i - 1][1], set[i][1]]
      cols.sort!
      rows.sort!

      if cols[0] == cols[1]
        (rows[0]..rows[1]).each { |r| matrix[r][cols[0]] = "#" }
      elsif rows[0] == rows[1]
        (cols[0]..cols[1]).each { |c| matrix[rows[0]][c] = "#" }
      end
    end
  end

  origin_col = 500 - lowest_col + extra_left_space
  matrix[0][origin_col] = "+"

  matrix
end

def drop_sand(matrix, origin_col)
  r = 0
  c = origin_col

  until r == matrix.length - 1
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

puts solve(file)
