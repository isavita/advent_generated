
def check_mas(grid : Array(String), x : Int32, y : Int32, dx : Int32, dy : Int32) : Bool
  return false if x < 0 || y < 0 || x >= grid.size || y >= grid[0].size

  word = "MAS"
  forward = true
  backward = true

  word.size.times do |i|
    new_x = x + (dx * i)
    new_y = y + (dy * i)
    if new_x < 0 || new_y < 0 || new_x >= grid.size || new_y >= grid[0].size
      forward = false
      break
    end
    if grid[new_x][new_y] != word[i]
      forward = false
    end
  end

  word.size.times do |i|
    new_x = x + (dx * i)
    new_y = y + (dy * i)
    if new_x < 0 || new_y < 0 || new_x >= grid.size || new_y >= grid[0].size
      backward = false
      break
    end
    if grid[new_x][new_y] != word[word.size - 1 - i]
      backward = false
    end
  end

  forward || backward
end

def check_xmas(grid : Array(String), x : Int32, y : Int32) : Bool
  check_mas(grid, x - 1, y - 1, 1, 1) && check_mas(grid, x - 1, y + 1, 1, -1) ||
  check_mas(grid, x + 1, y - 1, -1, 1) && check_mas(grid, x + 1, y + 1, -1, -1)
end

def count_xmas_patterns(grid : Array(String)) : Int32
  count = 0
  return 0 if grid.size < 3 || grid[0].size < 3

  (1...grid.size - 1).each do |i|
    (1...grid[i].size - 1).each do |j|
      if grid[i][j] == 'A' && check_xmas(grid, i, j)
        count += 1
      end
    end
  end
  count
end

grid = File.read("input.txt").lines.map(&.strip).reject(&.empty?)
count = count_xmas_patterns(grid)
puts "X-MAS patterns appear #{count} times in the word search"
