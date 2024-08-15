require "set"

struct Sensor
  property pos_x : Int32
  property pos_y : Int32
  property beacon_x : Int32
  property beacon_y : Int32
  property dist : Int32

  def initialize(@pos_x, @pos_y, @beacon_x, @beacon_y)
    @dist = (@pos_x - @beacon_x).abs + (@pos_y - @beacon_y).abs
  end
end

def impossible(sensors, y)
  pts = Set(Int32).new
  sensors.each do |s|
    dist = s.dist - (s.pos_y - y).abs
    (-dist..dist).each do |x|
      pts.add(s.pos_x + x)
    end
  end
  sensors.each do |s|
    if s.beacon_y == y
      pts.delete(s.beacon_x)
    end
  end
  pts.size
end

def read_all(path)
  File.read(path)
end

sensors = [] of Sensor
File.read("input.txt").each_line do |line|
  match = line.match(/Sensor at x=(\d+), y=(\d+): closest beacon is at x=(\d+), y=(\d+)/)
  if match
    sensors << Sensor.new(match[1].to_i32, match[2].to_i32, match[3].to_i32, match[4].to_i32)
  end
end
puts impossible(sensors, 2000000)