
def main
  grid = File.read_lines("input.txt")
  rows = grid.size
  cols = grid[0].size
  offs = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}]
  count = 0

  rows.times do |i|
    cols.times do |j|
      next unless grid[i][j] == '@'
      neigh = 0
      offs.each do |dx, dy|
        ni = i + dx
        nj = j + dy
        if ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] == '@'
          neigh += 1
        end
      end
      count += 1 if neigh < 4
    end
  end

  puts count
end

main
