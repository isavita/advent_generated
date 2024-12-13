
def solve(grid)
  rows = grid.length
  return 0 if rows == 0
  cols = grid[0].length
  visited = Array.new(rows) { Array.new(cols, false) }
  total_price = 0

  rows.times do |r|
    cols.times do |c|
      unless visited[r][c]
        area, perimeter = calculate_region(grid, r, c, visited)
        total_price += area * perimeter
      end
    end
  end
  total_price
end

def calculate_region(grid, row, col, visited)
  rows = grid.length
  cols = grid[0].length
  char = grid[row][col]
  area = 0
  perimeter = 0
  queue = [[row, col]]
  visited[row][col] = true

  until queue.empty?
    r, c = queue.shift
    area += 1
    is_border = r == 0 || r == rows - 1 || c == 0 || c == cols - 1

    [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]].each do |nr, nc|
      if nr >= 0 && nr < rows && nc >= 0 && nc < cols
        if grid[nr][nc] != char
          perimeter += 1
        elsif !visited[nr][nc]
          queue << [nr, nc]
          visited[nr][nc] = true
        end
      elsif is_border
        perimeter += 1
      end
    end
  end
  [area, perimeter]
end

grid = File.readlines("input.txt").map(&:chomp)
puts solve(grid)
