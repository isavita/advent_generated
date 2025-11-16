
struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

struct Sensor
  getter pos : Point
  getter beacon : Point
  getter dist : Int32

  def initialize(@pos : Point, @beacon : Point)
    @dist = (@pos.x - @beacon.x).abs + (@pos.y - @beacon.y).abs
  end
end

sensors = [] of Sensor
File.read_lines("input.txt").each do |line|
  if line.match(/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/)
    sx = $1.to_i
    sy = $2.to_i
    bx = $3.to_i
    by = $4.to_i
    sensors << Sensor.new(Point.new(sx, sy), Point.new(bx, by))
  end
end

maxcoord = 4_000_000
(0..maxcoord).each do |x|
  y = 0
  while y <= maxcoord
    detected = false
    skip = 0
    sensors.each do |s|
      d = (s.pos.x - x).abs + (s.pos.y - y).abs
      if d <= s.dist
        detected = true
        dy = s.dist - (s.pos.x - x).abs
        skip = {skip, dy + s.pos.y - y}.max
      end
    end
    unless detected
      puts x.to_i64 * 4_000_000 + y
      exit
    end
    y += Math.max(1, skip)
  end
end
