
require 'matrix'

Sensor = Struct.new(:pos, :beacon, :dist)

def read_input(file)
  File.read(file)
end

def manhattan(p, q)
  (p - q).map(&:abs).sum
end

def impossible(sensors, y)
  pts = {}
  sensors.each do |s|
    dist = s.dist - (s.pos[1] - y).abs
    (0..dist).each do |x|
      pts[s.pos[0] + x] = true
      pts[s.pos[0] - x] = true
    end
  end
  sensors.each do |s|
    pts.delete(s.beacon[0]) if s.beacon[1] == y
  end
  pts.size
end

input = read_input('input.txt').split("\n")
sensors = input.map do |line|
  pos_x, pos_y, beacon_x, beacon_y = line.scan(/\d+/).map(&:to_i)
  Sensor.new(Vector[pos_x, pos_y], Vector[beacon_x, beacon_y], manhattan(Vector[pos_x, pos_y], Vector[beacon_x, beacon_y]))
end

puts impossible(sensors, 2000000)
