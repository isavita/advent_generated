
require 'set'

def manhattan_distance(x1, y1, x2, y2)
  (x1 - x2).abs + (y1 - y2).abs
end

def parse_input(file_path)
  sensors = []
  beacons = Set.new

  File.foreach(file_path) do |line|
    line =~ /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/
    sensor_x, sensor_y, beacon_x, beacon_y = $1.to_i, $2.to_i, $3.to_i, $4.to_i
    sensors << [sensor_x, sensor_y, manhattan_distance(sensor_x, sensor_y, beacon_x, beacon_y)]
    beacons.add([beacon_x, beacon_y])
  end

  [sensors, beacons]
end

def count_impossible_positions(sensors, beacons, target_y)
  impossible_positions = Set.new

  sensors.each do |sensor_x, sensor_y, distance|
    y_diff = (target_y - sensor_y).abs
    next if y_diff > distance

    x_range = distance - y_diff
    (sensor_x - x_range..sensor_x + x_range).each do |x|
      impossible_positions.add([x, target_y]) unless beacons.include?([x, target_y])
    end
  end

  impossible_positions.size
end

def find_distress_beacon(sensors, max_coordinate)
  (0..max_coordinate).each do |y|
    ranges = []
    sensors.each do |sensor_x, sensor_y, distance|
      y_diff = (y - sensor_y).abs
      next if y_diff > distance

      x_range = distance - y_diff
      ranges << [sensor_x - x_range, sensor_x + x_range]
    end

    ranges.sort_by!(&:first)
    current_range = ranges.first

    ranges[1..-1].each do |range|
      if range.first <= current_range.last + 1
        current_range[1] = [current_range[1], range.last].max
      else
        return (current_range.last + 1) * 4000000 + y
      end
    end
  end

  nil
end

sensors, beacons = parse_input('input.txt')

# Part 1
target_y = 2000000
puts "Part 1: #{count_impossible_positions(sensors, beacons, target_y)}"

# Part 2
max_coordinate = 4000000
puts "Part 2: #{find_distress_beacon(sensors, max_coordinate)}"
