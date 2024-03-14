require 'set'

def erosion_level(geologic_index, depth)
  (geologic_index + depth) % 20183
end

def region_type(erosion_level)
  erosion_level % 3
end

def geologic_index(x, y, target, depth, memo)
  return memo[[x, y]] if memo.key?([x, y])

  if x == 0 && y == 0 || x == target[0] && y == target[1]
    index = 0
  elsif y == 0
    index = x * 16807
  elsif x == 0
    index = y * 48271
  else
    erosion1 = erosion_level(memo.fetch([x - 1, y], 0), depth)
    erosion2 = erosion_level(memo.fetch([x, y - 1], 0), depth)
    index = erosion1 * erosion2
  end

  memo[[x, y]] = index
  index
end

def valid_tools(region_type)
  case region_type
  when 0 # rocky
    [0, 1] # gear or torch
  when 1 # wet
    [0, 2] # gear or neither
  when 2 # narrow
    [1, 2] # torch or neither
  end
end

def dijkstra(cave, target, depth, memo)
  distances = {}
  distances[[0, 0, 1]] = 0 # start with torch
  queue = [[0, [0, 0, 1]]]

  until queue.empty?
    _, state = queue.min_by(&:first)
    queue.reject! { |d, s| s == state }

    x, y, tool = state
    dist = distances[state]

    return dist if x == target[0] && y == target[1] && tool == 1

    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dx, dy|
      nx, ny = x + dx, y + dy
      next if nx < 0 || ny < 0

      geo_index = cave.fetch([nx, ny]) do
        geologic_index(nx, ny, target, depth, memo)
        memo[[nx, ny]]
      end
      erosion = erosion_level(geo_index, depth)
      region_type = region_type(erosion)

      valid_tools(region_type).each do |new_tool|
        new_state = [nx, ny, new_tool]
        new_dist = dist + (new_tool == tool ? 1 : 8)

        if !distances.key?(new_state) || new_dist < distances[new_state]
          distances[new_state] = new_dist
          queue << [new_dist, new_state]
        end
      end
    end
  end
end

input = File.read('input.txt').split("\n")
depth = input[0].split(': ')[1].to_i
target = input[1].split(': ')[1].split(',').map(&:to_i)

cave = {}
memo = {}

(0..target[0] * 5).each do |x|
  (0..target[1] * 5).each do |y|
    geo_index = geologic_index(x, y, target, depth, memo)
    erosion = erosion_level(geo_index, depth)
    cave[[x, y]] = region_type(erosion)
  end
end

minutes = dijkstra(cave, target, depth, memo)
puts minutes
