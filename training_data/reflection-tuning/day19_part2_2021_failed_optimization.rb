require 'set'

def parse_input(input)
  input.split("\n\n").map do |scanner|
    scanner.lines[1..].map { |line| line.strip.split(',').map(&:to_i) }
  end
end

def rotations
  @rotations ||= [
    lambda { |x, y, z| [x, y, z] },
    lambda { |x, y, z| [x, z, -y] },
    lambda { |x, y, z| [x, -y, -z] },
    lambda { |x, y, z| [x, -z, y] },
    lambda { |x, y, z| [-x, y, -z] },
    lambda { |x, y, z| [-x, z, y] },
    lambda { |x, y, z| [-x, -y, z] },
    lambda { |x, y, z| [-x, -z, -y] },
    lambda { |x, y, z| [y, z, x] },
    lambda { |x, y, z| [y, x, -z] },
    lambda { |x, y, z| [y, -z, -x] },
    lambda { |x, y, z| [y, -x, z] },
    lambda { |x, y, z| [-y, z, -x] },
    lambda { |x, y, z| [-y, x, z] },
    lambda { |x, y, z| [-y, -z, x] },
    lambda { |x, y, z| [-y, -x, -z] },
    lambda { |x, y, z| [z, x, y] },
    lambda { |x, y, z| [z, y, -x] },
    lambda { |x, y, z| [z, -x, -y] },
    lambda { |x, y, z| [z, -y, x] },
    lambda { |x, y, z| [-z, x, -y] },
    lambda { |x, y, z| [-z, y, x] },
    lambda { |x, y, z| [-z, -x, y] },
    lambda { |x, y, z| [-z, -y, -x] }
  ]
end

def find_overlap(scanner1, scanner2)
  s1_set = Set.new(scanner1)
  
  rotations.each do |rotation|
    rotated = scanner2.map { |beacon| rotation.call(*beacon) }
    
    s1_set.each do |b1|
      rotated.each do |b2|
        dx, dy, dz = b1.zip(b2).map { |a, b| a - b }
        transformed = rotated.map { |x, y, z| [x + dx, y + dy, z + dz] }
        
        overlap = (s1_set & transformed.to_set)
        return [transformed, [dx, dy, dz]] if overlap.size >= 12
      end
    end
  end
  
  nil
end

def solve(scanners)
  aligned = [scanners.shift]
  positions = [[0, 0, 0]]
  
  until scanners.empty?
    scanner = scanners.shift
    found_match = false
    
    aligned.each do |ref_scanner|
      result = find_overlap(ref_scanner, scanner)
      if result
        transformed, position = result
        aligned << transformed
        positions << position
        found_match = true
        break
      end
    end
    
    scanners << scanner unless found_match
  end
  
  [aligned.flatten(1).uniq, positions]
end

def manhattan_distance(p1, p2)
  p1.zip(p2).map { |a, b| (a - b).abs }.sum
end

input = File.read('input.txt')
scanners = parse_input(input)

beacons, scanner_positions = solve(scanners)

puts "Part 1: #{beacons.size}"

max_distance = scanner_positions.combination(2).map { |p1, p2| manhattan_distance(p1, p2) }.max
puts "Part 2: #{max_distance}"
