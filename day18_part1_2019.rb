
require 'set'

def read_map
  File.read("input.txt").split("\n").map(&:chars)
end

def find_start(map)
  map.each_with_index do |row, y|
    x = row.index('@')
    return [x, y] if x
  end
end

def neighbors(x, y)
  [[x, y - 1], [x, y + 1], [x - 1, y], [x + 1, y]]
end

def bfs(map, start)
  queue = [[start, 0, Set.new, Set.new]] # [position, steps, keys, doors]
  visited = Set.new
  all_keys = map.flatten.select { |c| ('a'..'z').include?(c) }.size
  best_path = Float::INFINITY

  while queue.any?
    (x, y), steps, keys, doors = queue.shift
    state = [[x, y], keys]
    next if visited.include?(state)
    visited.add(state)

    case map[y][x]
    when '#'
      next
    when 'a'..'z'
      keys = keys.dup.add(map[y][x])
      best_path = [best_path, steps].min if keys.size == all_keys
    when 'A'..'Z'
      doors = doors.dup.add(map[y][x].downcase)
      next unless keys.include?(map[y][x].downcase)
    end

    neighbors(x, y).each do |nx, ny|
      next if map[ny][nx] == '#'
      queue.push([[nx, ny], steps + 1, keys, doors])
    end
  end

  best_path
end

map = read_map
start = find_start(map)
puts bfs(map, start)
