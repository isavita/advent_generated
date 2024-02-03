
require 'set'

def read_map(file)
  map = File.read(file).split("\n")
  points = {}
  map.each_with_index do |row, y|
    row.chars.each_with_index do |char, x|
      points[char.to_i] = [x, y] if char =~ /\d/
    end
  end
  [map, points]
end

def bfs(map, start, goal)
  queue = [[start, 0]]
  visited = Set.new([start])
  until queue.empty?
    (x, y), steps = queue.shift
    return steps if [x, y] == goal
    [[0, 1], [1, 0], [0, -1], [-1, 0]].each do |dx, dy|
      nx, ny = x + dx, y + dy
      if nx >= 0 && ny >= 0 && ny < map.size && nx < map.first.size && map[ny][nx] != '#' && !visited.include?([nx, ny])
        visited.add([nx, ny])
        queue.push([[nx, ny], steps + 1])
      end
    end
  end
end

def shortest_path(map, points, return_to_start = false)
  distances = Hash.new { |h, k| h[k] = {} }
  points.keys.permutation(2).each do |a, b|
    distances[a][b] = bfs(map, points[a], points[b])
  end
  min_distance = Float::INFINITY
  (1..points.keys.max).to_a.permutation.each do |perm|
    path = [0] + perm
    path << 0 if return_to_start
    distance = path.each_cons(2).sum { |a, b| distances[a][b] }
    min_distance = [min_distance, distance].min
  end
  min_distance
end

map, points = read_map('input.txt')
puts shortest_path(map, points)
puts shortest_path(map, points, true)
