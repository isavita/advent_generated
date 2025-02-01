
size = 71
grid = Array.new(size) { Array.new(size, false) }
File.foreach("input.txt").with_index do |line, i|
  break if i >= 1024
  x, y = line.split(",").map(&:to_i)
  grid[y][x] = true if x >= 0 && x < size && y >= 0 && y < size
end

dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]
visited = Array.new(size) { Array.new(size, false) }

q = [[0, 0, 0]]
visited[0][0] = true

until q.empty?
  x, y, steps = q.shift
  if x == size - 1 && y == size - 1
    puts steps
    exit
  end
  dirs.each do |dx, dy|
    nx, ny = x + dx, y + dy
    if nx >= 0 && ny >= 0 && nx < size && ny < size && !grid[ny][nx] && !visited[ny][nx]
      visited[ny][nx] = true
      q << [nx, ny, steps + 1]
    end
  end
end
puts "No path"
