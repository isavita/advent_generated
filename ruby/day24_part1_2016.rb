
require 'set'

def find_shortest_path(map)
  locations = {}
  map.each_with_index do |row, y|
    row.chars.each_with_index do |char, x|
      locations[char] = [x, y] if char =~ /\d/
    end
  end

  distances = Hash.new { |h, k| h[k] = {} }
  locations.keys.each do |start|
    locations.keys.each do |finish|
      next if start == finish
      distances[start][finish] = distances[finish][start] = find_distance(map, locations[start], locations[finish])
    end
  end

  locations.keys.permutation.map do |path|
    path.each_cons(2).sum { |start, finish| distances[start][finish] }
  end.min
end

def find_distance(map, start, finish)
  queue = [[start, 0]]
  visited = Set.new

  until queue.empty?
    (x, y), steps = queue.shift
    return steps if [x, y] == finish

    [[1, 0], [-1, 0], [0, 1], [0, -1]].each do |dx, dy|
      new_x, new_y = x + dx, y + dy
      next if new_x < 0 || new_x >= map[0].size || new_y < 0 || new_y >= map.size
      next if map[new_y][new_x] == '#'
      next if visited.include?([new_x, new_y])

      visited << [new_x, new_y]
      queue << [[new_x, new_y], steps + 1]
    end
  end
end

map = File.readlines('input.txt').map(&:chomp)
puts find_shortest_path(map)
