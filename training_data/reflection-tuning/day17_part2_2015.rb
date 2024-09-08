# Read container sizes from input file
containers = File.readlines('input.txt').map(&:to_i)
target = 150

# Initialize variables
total_combinations = 0
min_containers = Float::INFINITY
min_container_combinations = 0

# Iterate through all possible combinations using bitmask
(1...(1 << containers.size)).each do |mask|
  volume = 0
  container_count = 0
  
  containers.each_with_index do |container, index|
    if mask & (1 << index) != 0
      volume += container
      container_count += 1
    end
  end
  
  if volume == target
    total_combinations += 1
    
    if container_count < min_containers
      min_containers = container_count
      min_container_combinations = 1
    elsif container_count == min_containers
      min_container_combinations += 1
    end
  end
end

puts "Part One: #{total_combinations}"
puts "Part Two: #{min_container_combinations}"
