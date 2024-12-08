
def check_mas(grid, x, y, dx, dy)
  word = "MAS"
  (0...word.length).all? do |i|
    nx, ny = x + dx * i, y + dy * i
    nx >= 0 && nx < grid.size && ny >= 0 && ny < grid[0].size && grid[nx][ny] == word[i]
  end || (0...word.length).all? do |i|
    nx, ny = x + dx * i, y + dy * i
    nx >= 0 && nx < grid.size && ny >= 0 && ny < grid[0].size && grid[nx][ny] == word[word.length - 1 - i]
  end
end

def check_xmas(grid, x, y)
  check_mas(grid, x - 1, y - 1, 1, 1) && check_mas(grid, x - 1, y + 1, 1, -1) ||
    check_mas(grid, x + 1, y - 1, -1, 1) && check_mas(grid, x + 1, y + 1, -1, -1)
end

def count_xmas_patterns(grid)
  (1...grid.size - 1).sum do |i|
    (1...grid[i].size - 1).count do |j|
      grid[i][j] == 'A' && check_xmas(grid, i, j)
    end
  end
end

grid = File.readlines('input.txt').map(&:chomp)
puts "X-MAS patterns appear #{count_xmas_patterns(grid)} times in the word search"
