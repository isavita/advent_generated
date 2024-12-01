# Read the input file
input = File.read("input.txt")

# Initialize arrays to hold the left and right lists
left_list = [] of Int32
right_list = [] of Int32

# Parse each line of the input
input.each_line do |line|
  # Split the line into two parts and convert them to integers
  left, right = line.split.map(&.to_i)
  # Append the integers to their respective lists
  left_list << left
  right_list << right
end

# Sort both lists
left_list.sort!
right_list.sort!

# Calculate the total distance
total_distance = 0
left_list.each_with_index do |left, index|
  right = right_list[index]
  total_distance += (left - right).abs
end

# Output the total distance
puts "Total distance: #{total_distance}"
