
def can_reach(grid : Array(Array(Bool))) : Bool
  n = grid.size
  return false if grid[0][0] || grid[n - 1][n - 1]
  dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]
  visited = Array.new(n) { Array.new(n, false) }
  q = Deque(Tuple(Int32, Int32)).new
  q.push({0, 0})
  visited[0][0] = true
  while !q.empty?
    c = q.shift
    return true if c == {n - 1, n - 1}
    dirs.each do |d|
      nx, ny = c[0] + d[0], c[1] + d[1]
      if nx >= 0 && ny >= 0 && nx < n && ny < n && !grid[ny][nx] && !visited[ny][nx]
        visited[ny][nx] = true
        q.push({nx, ny})
      end
    end
  end
  false
end

size = 71
grid = Array.new(size) { Array.new(size, false) }
File.open("input.txt") do |f|
  f.each_line.with_index do |line, i|
    x, y = line.split(",").map(&.to_i)
    if x >= 0 && x < size && y >= 0 && y < size
      grid[y][x] = true
    end
    unless can_reach(grid)
      puts "#{x},#{y}"
      exit
    end
  end
end
puts "No cutoff found"
