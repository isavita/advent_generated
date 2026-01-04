
struct Point
  property x : Int32
  property y : Int32
  def initialize(@x, @y); end
end

def main
  points = [] of Point
  File.read("input.txt").lines.each do |line|
    line = line.strip
    next if line.empty?
    parts = line.split(',')
    next unless parts.size == 2
    begin
      points << Point.new(parts[0].to_i, parts[1].to_i)
    rescue
    end
  end

  max_area = 0_i64
  i = 0
  while i < points.size
    j = i
    while j < points.size
      dx = (points[i].x - points[j].x).abs.to_i64 + 1
      dy = (points[i].y - points[j].y).abs.to_i64 + 1
      area = dx * dy
      max_area = area if area > max_area
      j += 1
    end
    i += 1
  end

  puts "Largest area: #{max_area}"
end

main
