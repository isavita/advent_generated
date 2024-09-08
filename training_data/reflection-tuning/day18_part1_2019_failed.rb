require 'set'
require 'priority_queue'

def shortest_path(map)
  start = find_start(map)
  keys = find_keys(map)
  all_keys = (1 << keys.size) - 1

  queue = PriorityQueue.new
  queue.push([start, 0, 0], 0)
  visited = Set.new

  while !queue.empty?
    (pos, collected, steps), _ = queue.pop
    state = [pos, collected]

    return steps if collected == all_keys
    next if visited.include?(state)

    visited.add(state)

    keys.each_with_index do |(key_pos, key), index|
      next if (collected & (1 << index)) != 0

      dist = bfs_distance(map, pos, key_pos, collected)
      next unless dist

      new_collected = collected | (1 << index)
      new_steps = steps + dist
      queue.push([key_pos, new_collected, new_steps], new_steps)
    end
  end

  -1 # No path found
end

def find_start(map)
  map.each_with_index do |row, y|
    x = row.index('@')
    return [y, x] if x
  end
end

def find_keys(map)
  keys = {}
  map.each_with_index do |row, y|
    row.each_char.with_index do |char, x|
      keys[[y, x]] = char if ('a'..'z').include?(char)
    end
  end
  keys
end

def bfs_distance(map, start, goal, keys)
  queue = [[start, 0]]
  visited = Set.new

  while !queue.empty?
    pos, dist = queue.shift
    return dist if pos == goal

    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dy, dx|
      ny, nx = pos[0] + dy, pos[1] + dx
      next if ny < 0 || ny >= map.size || nx < 0 || nx >= map[0].size
      next if visited.include?([ny, nx])
      
      char = map[ny][nx]
      next if char == '#'
      next if char.upcase == char && char != '.' && (keys & (1 << (char.ord - 'A'.ord))) == 0

      visited.add([ny, nx])
      queue.push([[ny, nx], dist + 1])
    end
  end

  nil # No path found
end

# Read input
map = ARGF.readlines.map(&:chomp)

# Solve and print result
puts shortest_path(map)
