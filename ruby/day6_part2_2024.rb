
def solve
  grid = File.readlines("input.txt").map(&:strip).map(&:chars)
  h = grid.size
  w = grid[0].size

  start_x, start_y, start_dir = find_start(grid)
  grid[start_y][start_x] = '.'

  count = 0
  (0...h).each do |y|
    (0...w).each do |x|
      next if x == start_x && y == start_y
      next if grid[y][x] != '.'

      grid[y][x] = '#'
      count += 1 if loops?(grid, start_x, start_y, start_dir)
      grid[y][x] = '.'
    end
  end
  puts count
end

def find_start(grid)
    h = grid.size
    w = grid[0].size
  (0...h).each do |y|
    (0...w).each do |x|
      case grid[y][x]
      when '^' then return [x, y, 0]
      when '>' then return [x, y, 1]
      when 'v' then return [x, y, 2]
      when '<' then return [x, y, 3]
      end
    end
  end
end


def loops?(grid, start_x, start_y, start_dir)
  h = grid.size
  w = grid[0].size
  dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]]
  x, y, dir = start_x, start_y, start_dir
  seen = {}

  2_000_000.times do
    state = [x, y, dir]
    return true if seen[state]
    seen[state] = true

    dx, dy = dirs[dir]
    nx, ny = x + dx, y + dy

    return false if nx < 0 || nx >= w || ny < 0 || ny >= h
    if grid[ny][nx] == '#'
      dir = (dir + 1) % 4
    else
      x, y = nx, ny
    end
  end
  false
end

solve
