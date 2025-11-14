
require "set"

def dijkstra(grid)
  n = grid.size
  m = grid[0].size
  dist = Array.new(n) { Array.new(m, Int32::MAX) }
  dist[0][0] = 0
  queue = Set.new([{0, 0}])

  until queue.empty?
    x, y = queue.min_by { |i, j| dist[i][j] }
    queue.delete({x, y})

    [{0, 1}, {0, -1}, {1, 0}, {-1, 0}].each do |dx, dy|
      nx, ny = x + dx, y + dy
      next if nx < 0 || nx >= n || ny < 0 || ny >= m
      new_dist = dist[x][y] + grid[nx][ny]
      if new_dist < dist[nx][ny]
        dist[nx][ny] = new_dist
        queue.add({nx, ny})
      end
    end
  end

  dist[n - 1][m - 1]
end

grid = File.read_lines("input.txt").map { |line| line.chars.map(&.to_i) }
puts dijkstra(grid)
