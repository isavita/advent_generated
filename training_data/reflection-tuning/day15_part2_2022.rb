class Sensor
  attr_reader :x, :y, :range

  def initialize(sx, sy, bx, by)
    @x, @y = sx, sy
    @range = (sx - bx).abs + (sy - by).abs
  end

  def covers?(px, py)
    (x - px).abs + (y - py).abs <= range
  end
end

sensors = File.readlines('input.txt', chomp: true).map do |line|
  sx, sy, bx, by = line.scan(/-?\d+/).map(&:to_i)
  Sensor.new(sx, sy, bx, by)
end

# Part 1
def count_covered_positions(sensors, y)
  ranges = sensors.map do |s|
    dy = (s.y - y).abs
    dx = s.range - dy
    next if dx < 0
    (s.x - dx)..(s.x + dx)
  end.compact.sort_by(&:begin)

  covered = 0
  current_end = ranges.first.begin - 1
  ranges.each do |range|
    covered += [range.end, current_end].max - [range.begin, current_end].max + 1 if range.begin > current_end
    current_end = [current_end, range.end].max
  end
  covered
end

puts "Part 1: #{count_covered_positions(sensors, 2000000)}"

# Part 2
def find_distress_beacon(sensors, max_coord)
  sensors.each do |s|
    (s.range + 1).times do |dx|
      dy = s.range + 1 - dx
      [1, -1].each do |sign_x|
        [1, -1].each do |sign_y|
          x = s.x + dx * sign_x
          y = s.y + dy * sign_y
          next if x < 0 || y < 0 || x > max_coord || y > max_coord
          return [x, y] if sensors.none? { |other| other.covers?(x, y) }
        end
      end
    end
  end
end

x, y = find_distress_beacon(sensors, 4000000)
puts "Part 2: #{x * 4000000 + y}"
