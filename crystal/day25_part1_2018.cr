
# Function to calculate the Manhattan distance between two points
def manhattan_distance(point1 : Array(Int32), point2 : Array(Int32)) : Int32
  point1.zip(point2).map { |a, b| (a - b).abs }.sum
end

# Function to find the number of constellations
def count_constellations(points : Array(Array(Int32))) : Int32
  visited = Set(Int32).new
  constellations = 0

  points.each_with_index do |point, index|
    next if visited.includes?(index)

    # Start a new constellation
    constellations += 1
    queue = [index]

    while !queue.empty?
      current_index = queue.pop
      visited.add(current_index)

      points.each_with_index do |other_point, other_index|
        if !visited.includes?(other_index) && manhattan_distance(points[current_index], other_point) <= 3
          queue << other_index
        end
      end
    end
  end

  constellations
end

# Read input from the file
def read_input(file_name : String) : Array(Array(Int32))
  File.read_lines(file_name).map do |line|
    line.split(",").map(&.to_i)
  end
end

# Main execution
points = read_input("input.txt")
constellation_count = count_constellations(points)
puts constellation_count
