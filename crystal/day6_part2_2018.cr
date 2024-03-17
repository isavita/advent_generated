# Read input from "input.txt"
input = File.read_lines("input.txt")

# Parse input coordinates
coords = input.map { |line| line.split(", ").map(&.to_i) }

# Find the maximum and minimum coordinates
max_x = coords.map(&.[0]).max || 0
max_y = coords.map(&.[1]).max || 0
min_x = coords.map(&.[0]).min || 0
min_y = coords.map(&.[1]).min || 0

# Function to calculate Manhattan distance
def manhattan_distance(x1, y1, x2, y2)
  (x1 - x2).abs + (y1 - y2).abs
end

# Part 1: Find the size of the largest area that isn't infinite
def part1(coords, max_x, max_y, min_x, min_y)
  areas = Array.new(coords.size, 0)
  (min_x..max_x).each do |x|
    (min_y..max_y).each do |y|
      closest = -1
      min_distance = Int32::MAX
      coords.each_with_index do |coord, i|
        distance = manhattan_distance(x, y, coord[0], coord[1])
        if distance < min_distance
          min_distance = distance
          closest = i
        elsif distance == min_distance
          closest = -1
        end
      end
      if closest >= 0
        areas[closest] += 1
      end
    end
  end

  # Exclude coordinates on the edge, as their areas are infinite
  areas.each_with_index.reject { |area, i| coords[i][0] == min_x || coords[i][0] == max_x || coords[i][1] == min_y || coords[i][1] == max_y }
       .map(&.[0])
       .max || 0
end

# Part 2: Find the size of the region containing all locations which have a total distance to all given coordinates of less than 10000
def part2(coords, max_x, max_y, min_x, min_y)
  region_size = 0
  (min_x..max_x).each do |x|
    (min_y..max_y).each do |y|
      total_distance = coords.sum { |coord| manhattan_distance(x, y, coord[0], coord[1]) }
      region_size += 1 if total_distance < 10000
    end
  end
  region_size
end

puts "Part 1: #{part1(coords, max_x, max_y, min_x, min_y)}"
puts "Part 2: #{part2(coords, max_x, max_y, min_x, min_y)}"