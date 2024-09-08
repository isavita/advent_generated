require 'set'

def parse_maze(input)
  maze = input.map(&:chars)
  portals = {}
  height, width = maze.size, maze.first.size

  (0...height).each do |y|
    (0...width).each do |x|
      if maze[y][x] =~ /[A-Z]/
        [[0, 1], [1, 0]].each do |dy, dx|
          ny, nx = y + dy, x + dx
          if ny < height && nx < width && maze[ny][nx] =~ /[A-Z]/
            name = [maze[y][x], maze[ny][nx]].join
            portal_pos = [y - dy, x - dx] if y - dy >= 0 && x - dx >= 0 && maze[y - dy][x - dx] == '.'
            portal_pos = [ny + dy, nx + dx] if !portal_pos && ny + dy < height && nx + dx < width && maze[ny + dy][nx + dx] == '.'
            if portal_pos
              portals[name] ||= []
              portals[name] << portal_pos
            end
          end
        end
      end
    end
  end

  [maze, portals]
end

def solve(maze, portals, recursive = false)
  start = portals['AA'].first
  target = portals['ZZ'].first
  queue = [[start, 0, 0]]  # [position, steps, level]
  visited = Set.new
  height, width = maze.size, maze.first.size

  while !queue.empty?
    pos, steps, level = queue.shift
    y, x = pos

    return steps if pos == target && (!recursive || level == 0)
    next if visited.include?([pos, level])
    visited.add([pos, level])

    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dy, dx|
      ny, nx = y + dy, x + dx
      if ny.between?(0, height - 1) && nx.between?(0, width - 1) && maze[ny][nx] == '.'
        queue << [[ny, nx], steps + 1, level]
      end
    end

    portals.each do |name, locations|
      next if name == 'AA' || name == 'ZZ'
      if locations.include?(pos)
        other_pos = locations.find { |p| p != pos }
        is_outer = pos[0] == 2 || pos[1] == 2 || pos[0] == height - 3 || pos[1] == width - 3
        if recursive
          if (is_outer && level > 0) || (!is_outer && level >= 0)
            new_level = is_outer ? level - 1 : level + 1
            queue << [other_pos, steps + 1, new_level]
          end
        else
          queue << [other_pos, steps + 1, level]
        end
      end
    end
  end

  nil
end

input = File.readlines('input.txt', chomp: true)
maze, portals = parse_maze(input)

puts "Part 1: #{solve(maze, portals)}"
puts "Part 2: #{solve(maze, portals, true)}"
