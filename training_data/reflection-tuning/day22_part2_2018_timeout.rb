require 'set'

class Cave
  ROCKY = 0
  WET = 1
  NARROW = 2

  def initialize(depth, target_x, target_y)
    @depth = depth
    @target_x = target_x
    @target_y = target_y
    @erosion_levels = {}
  end

  def erosion_level(x, y)
    @erosion_levels[[x, y]] ||= (geologic_index(x, y) + @depth) % 20183
  end

  def geologic_index(x, y)
    return 0 if (x == 0 && y == 0) || (x == @target_x && y == @target_y)
    return x * 16807 if y == 0
    return y * 48271 if x == 0
    erosion_level(x-1, y) * erosion_level(x, y-1)
  end

  def type(x, y)
    erosion_level(x, y) % 3
  end

  def risk_level
    (0..@target_y).sum do |y|
      (0..@target_x).sum { |x| type(x, y) }
    end
  end

  def shortest_path
    start = [0, 0, 1]  # x, y, tool (1 = torch)
    target = [@target_x, @target_y, 1]
    distances = {start => 0}
    queue = [[0, start]]
    seen = Set.new

    while !queue.empty?
      dist, (x, y, tool) = queue.shift
      pos = [x, y, tool]
      next if seen.include?(pos)
      seen.add(pos)

      return dist if pos == target

      [[x-1, y], [x+1, y], [x, y-1], [x, y+1]].each do |nx, ny|
        next if nx < 0 || ny < 0
        nt = type(nx, ny)
        if tool != nt
          ndist = dist + 1
          npos = [nx, ny, tool]
          if ndist < (distances[npos] || Float::INFINITY)
            distances[npos] = ndist
            queue << [ndist, npos]
          end
        end
      end

      [0, 1, 2].each do |nt|
        next if nt == type(x, y)
        ndist = dist + 7
        npos = [x, y, nt]
        if ndist < (distances[npos] || Float::INFINITY)
          distances[npos] = ndist
          queue << [ndist, npos]
        end
      end

      queue.sort_by!(&:first)
    end
  end
end

depth, target = File.read('input.txt').split("\n").map { |line| line.split(': ')[1] }
depth = depth.to_i
target_x, target_y = target.split(',').map(&:to_i)

cave = Cave.new(depth, target_x, target_y)
puts "Part 1: #{cave.risk_level}"
puts "Part 2: #{cave.shortest_path}"
