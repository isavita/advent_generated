require 'set'
require 'algorithms'

ROCKY, WET, NARROW = 0, 1, 2
TORCH, CLIMBING_GEAR, NEITHER = 0, 1, 2
ALLOWED_TOOLS = {
  ROCKY => [CLIMBING_GEAR, TORCH],
  WET => [CLIMBING_GEAR, NEITHER],
  NARROW => [TORCH, NEITHER]
}

class Cave
  def initialize(depth, target)
    @depth = depth
    @target = target
    @erosion_levels = {}
  end

  def type(x, y)
    erosion_level(x, y) % 3
  end

  def erosion_level(x, y)
    @erosion_levels[[x, y]] ||= (geologic_index(x, y) + @depth) % 20183
  end

  def geologic_index(x, y)
    return 0 if [0, 0] == [x, y] || [x, y] == @target
    return x * 16807 if y == 0
    return y * 48271 if x == 0
    erosion_level(x-1, y) * erosion_level(x, y-1)
  end
end

def shortest_path(cave, target)
  queue = Containers::PriorityQueue.new
  queue.push([0, 0, TORCH, 0], 0)
  visited = Set.new

  while !queue.empty?
    x, y, tool, time = queue.pop

    return time if [x, y] == target && tool == TORCH

    state = [x, y, tool]
    next if visited.include?(state)
    visited.add(state)

    # Try changing tools
    ALLOWED_TOOLS[cave.type(x, y)].each do |new_tool|
      next if new_tool == tool
      queue.push([x, y, new_tool, time + 7], time + 7 + manhattan_distance(x, y, *target))
    end

    # Try moving to adjacent cells
    [[x-1, y], [x+1, y], [x, y-1], [x, y+1]].each do |nx, ny|
      next if nx < 0 || ny < 0
      next unless ALLOWED_TOOLS[cave.type(nx, ny)].include?(tool)
      queue.push([nx, ny, tool, time + 1], time + 1 + manhattan_distance(nx, ny, *target))
    end
  end
end

def manhattan_distance(x1, y1, x2, y2)
  (x1 - x2).abs + (y1 - y2).abs
end

depth = 5913
target = [8, 701]

cave = Cave.new(depth, target)
puts shortest_path(cave, target)
