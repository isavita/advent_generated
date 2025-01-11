
record Point, x : Int32, y : Int32, z : Int32 do
  def +(other : Point)
    Point.new(x + other.x, y + other.y, z + other.z)
  end

  def -(other : Point)
    Point.new(x - other.x, y - other.y, z - other.z)
  end

  def rotate(rotation)
    case rotation
    when 0  then Point.new( x,  y,  z)
    when 1  then Point.new( x, -z,  y)
    when 2  then Point.new( x, -y, -z)
    when 3  then Point.new( x,  z, -y)
    when 4  then Point.new(-x, -y,  z)
    when 5  then Point.new(-x, -z, -y)
    when 6  then Point.new(-x,  y, -z)
    when 7  then Point.new(-x,  z,  y)
    when 8  then Point.new( y, -x,  z)
    when 9  then Point.new( y, -z, -x)
    when 10 then Point.new( y,  x, -z)
    when 11 then Point.new( y,  z,  x)
    when 12 then Point.new(-y,  x,  z)
    when 13 then Point.new(-y, -z,  x)
    when 14 then Point.new(-y, -x, -z)
    when 15 then Point.new(-y,  z, -x)
    when 16 then Point.new( z,  y, -x)
    when 17 then Point.new( z,  x,  y)
    when 18 then Point.new( z, -y,  x)
    when 19 then Point.new( z, -x, -y)
    when 20 then Point.new(-z,  y,  x)
    when 21 then Point.new(-z, -x,  y)
    when 22 then Point.new(-z, -y, -x)
    when 23 then Point.new(-z,  x, -y)
    else         raise "Invalid rotation"
    end
  end

  def manhattan_distance(other : Point)
    (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
  end
end

def solve
  scanners = [] of Array(Point)
  File.open("./input.txt") do |file|
    scanner = [] of Point
    file.each_line do |line|
      if line.starts_with?("---")
        next
      elsif line.empty?
        scanners << scanner
        scanner = [] of Point
      else
        x, y, z = line.split(',').map(&.to_i32)
        scanner << Point.new(x, y, z)
      end
    end
    scanners << scanner
  end

  scanner_positions = [] of Point
  scanner_positions << Point.new(0, 0, 0)
  beacons = scanners[0].to_set

  queue = [] of Int32
  (1...scanners.size).each { |i| queue << i }

  until queue.empty?
    i = queue.shift
    found_match = false
    scanners[i].each_with_index do |_, index|
      24.times do |rotation|
        rotated_scanner = scanners[i].map { |p| p.rotate(rotation) }
        counts = Hash(Point, Int32).new(0)
        beacons.each do |b1|
          rotated_scanner.each do |b2|
            counts[b1 - b2] += 1
          end
        end

        counts.each do |diff, count|
          if count >= 12
            scanner_positions << diff
            rotated_scanner.each { |p| beacons << p + diff }
            scanners[i] = rotated_scanner
            found_match = true
            break
          end
        end
        break if found_match
      end
      break if found_match
    end
    queue << i unless found_match
  end

  puts "Part 1: #{beacons.size}"

  max_distance = 0
  scanner_positions.each_with_index do |s1, i|
    scanner_positions.each_with_index do |s2, j|
      next if i == j
      max_distance = Math.max(max_distance, s1.manhattan_distance(s2))
    end
  end

  puts "Part 2: #{max_distance}"
end

solve
