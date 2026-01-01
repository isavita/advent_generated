
#!/usr/bin/env ruby
def read_points
  points = []
  File.foreach('input.txt') do |line|
    line = line.strip
    next if line.empty?
    a, b = line.split(',')
    next unless a && b
    begin
      points << [Integer(a), Integer(b)]
    rescue ArgumentError
      # skip invalid lines
    end
  end
  points
end

def max_area(points)
  max = 0
  i = 0
  n = points.length
  while i < n
    xi, yi = points[i]
    j = i
    while j < n
      xj, yj = points[j]
      dx = (xi - xj).abs + 1
      dy = (yi - yj).abs + 1
      area = dx * dy
      max = area if area > max
      j += 1
    end
    i += 1
  end
  max
end

if __FILE__ == $0
  pts = read_points
  puts max_area(pts)
end
