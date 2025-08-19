
require "set"

def read_maze
  File.read_lines("input.txt").map(&.chars)
end

def find_portals(maze)
  h = maze.size
  w = maze[0].size
  portals = Hash(String, Array(Tuple(Int32, Int32))).new { |hh, k| hh[k] = [] of Tuple(Int32, Int32) }
  portal_pos = Hash(Tuple(Int32, Int32), String).new

  (0...h).each do |y|
    (0...w).each do |x|
      c = maze[y][x]
      next unless ('A' <= c && c <= 'Z')
      if x + 1 < w && ('A' <= maze[y][x + 1] && maze[y][x + 1] <= 'Z')
        name = "#{c}#{maze[y][x + 1]}"
        if x + 2 < w && maze[y][x + 2] == '.'
          pos = {x + 2, y}
          portals[name] << pos
          portal_pos[pos] = name
        elsif x > 0 && maze[y][x - 1] == '.'
          pos = {x - 1, y}
          portals[name] << pos
          portal_pos[pos] = name
        end
      end
      if y + 1 < h && ('A' <= maze[y + 1][x] && maze[y + 1][x] <= 'Z')
        name = "#{c}#{maze[y + 1][x]}"
        if y + 2 < h && maze[y + 2][x] == '.'
          pos = {x, y + 2}
          portals[name] << pos
          portal_pos[pos] = name
        elsif y > 0 && maze[y - 1][x] == '.'
          pos = {x, y - 1}
          portals[name] << pos
          portal_pos[pos] = name
        end
      end
    end
  end
  {portals, portal_pos}
end

def bfs(maze, portals, portal_pos, start, goal)
  w = maze[0].size
  h = maze.size
  q = [] of Tuple(Int32, Int32, Int32)
  q << {start[0], start[1], 0}
  head = 0
  visited = Set(Tuple(Int32, Int32)).new
  visited << start

  while head < q.size
    x, y, d = q[head]
    head += 1

    [{-1, 0}, {1, 0}, {0, -1}, {0, 1}].each do |dx, dy|
      nx = x + dx
      ny = y + dy
      next unless 0 <= nx && nx < w && 0 <= ny && ny < h
      next unless maze[ny][nx] == '.'
      return d + 1 if {nx, ny} == goal
      pos = {nx, ny}
      unless visited.includes?(pos)
        visited << pos
        q << {nx, ny, d + 1}
      end
    end

    if portal_pos.has_key?({x, y})
      name = portal_pos[{x, y}]
      portals[name].each do |px, py|
        next if px == x && py == y
        pos = {px, py}
        unless visited.includes?(pos)
          visited << pos
          q << {px, py, d + 1}
        end
      end
    end
  end
  -1
end

def main
  maze = read_maze
  portals, portal_pos = find_portals(maze)
  start = portals["AA"][0]
  goal = portals["ZZ"][0]
  puts bfs(maze, portals, portal_pos, start, goal)
end

main
