def manhattan_distance(x1, y1, x2, y2)
  (x1 - x2).abs + (y1 - y2).abs
end

def merge_ranges(ranges)
  sorted = ranges.sort_by(&:begin)
  merged = []

  sorted.each do |range|
    if merged.empty? || merged.last.end < range.begin
      merged << range
    else
      last = merged.pop
      merged << (last.begin..[last.end, range.end].max)
    end
  end

  merged
end

def count_positions(ranges)
  merge_ranges(ranges).sum { |range| range.size }
end

def parse_input(input)
  input.map do |line|
    line.scan(/-?\d+/).map(&:to_i)
  end
end

def calculate_coverage(sensors, y)
  ranges = []
  sensors.each do |sx, sy, bx, by|
    distance = manhattan_distance(sx, sy, bx, by)
    y_distance = (sy - y).abs
    if y_distance <= distance
      x_range = distance - y_distance
      ranges << ((sx - x_range)..(sx + x_range))
    end
  end
  ranges
end

# Read input
input = File.readlines('input.txt').map(&:chomp)
sensors = parse_input(input)

# Calculate coverage for y=2000000
y = 2000000
coverage = calculate_coverage(sensors, y)

# Count positions that cannot contain a beacon
result = count_positions(coverage)

puts "In the row where y=#{y}, #{result} positions cannot contain a beacon."
