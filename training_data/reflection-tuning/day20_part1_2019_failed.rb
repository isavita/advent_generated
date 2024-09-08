def build_graph(maze, portals)
  graph = Hash.new { |h, k| h[k] = [] }
  
  # Initialize all positions in the maze as vertices
  maze.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      if cell == '.'
        pos = [y, x]
        graph[pos] = []
        [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dy, dx|
          ny, nx = y + dy, x + dx
          if maze[ny][nx] == '.'
            graph[pos] << [ny, nx]
          end
        end
      end
    end
  end

  # Add portal connections
  portals.each_value do |portal_positions|
    if portal_positions.size == 2
      pos1, pos2 = portal_positions
      graph[pos1] << pos2
      graph[pos2] << pos1
    end
  end

  graph
end

def bfs(graph, start, goal)
  queue = [[start, 0]]
  visited = Set.new([start])

  while !queue.empty?
    position, distance = queue.shift

    return distance if position == goal

    graph[position].each do |neighbor|
      unless visited.include?(neighbor)
        visited.add(neighbor)
        queue << [neighbor, distance + 1]
      end
    end
  end

  nil  # No path found
end

def solve_maze(input)
  maze = input.split("\n").map(&:chars)
  portals = find_portals(maze)
  start = portals['AA'][0]
  goal = portals['ZZ'][0]
  graph = build_graph(maze, portals)
  bfs(graph, start, goal)
end

def find_portals(maze)
  portals = Hash.new { |h, k| h[k] = [] }
  height, width = maze.size, maze[0].size

  (0...height).each do |y|
    (0...width).each do |x|
      if maze[y][x] =~ /[A-Z]/
        [[0, 1], [1, 0]].each do |dy, dx|
          ny, nx = y + dy, x + dx
          if ny < height && nx < width && maze[ny][nx] =~ /[A-Z]/
            label = [maze[y][x], maze[ny][nx]].join
            portal_pos = if y > 0 && maze[y-1][x] == '.'
                           [y-1, x]
                         elsif ny < height-1 && maze[ny+1][nx] == '.'
                           [ny+1, nx]
                         elsif x > 0 && maze[y][x-1] == '.'
                           [y, x-1]
                         elsif nx < width-1 && maze[y][nx+1] == '.'
                           [y, nx+1]
                         end
            portals[label] << portal_pos if portal_pos
          end
        end
      end
    end
  end
  portals
end

# Example usage
input = File.read('maze_input.txt')
puts solve_maze(input)
