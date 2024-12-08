
grid = File.readlines('input.txt').map(&:chars)
h = grid.size
w = grid[0].size

start = grid.each_with_index.flat_map do |row, y|
  row.each_with_index.select { |c, x| %w[^ > v <].include?(c) }.map { |c, x| [x, y, c] }
end.first

x, y, dir = start

dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]
dir_idx = case dir
          when '^' then 0
          when '>' then 1
          when 'v' then 2
          when '<' then 3
          end

visited = { [x, y] => true }

loop do
  nx, ny = x + dirs[dir_idx][0], y + dirs[dir_idx][1]
  break if nx < 0 || nx >= w || ny < 0 || ny >= h

  if grid[ny][nx] == '#'
    dir_idx = (dir_idx + 1) % 4
    next
  end

  x, y = nx, ny
  visited[[x, y]] = true
end

puts visited.size
