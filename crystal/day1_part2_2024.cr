# Read the input file
input = File.read("input.txt")

# Initialize arrays to hold the left and right lists
left_list = [] of Int32
right_list = [] of Int32

# Process each line of the input
input.each_line do |line|
  # Split the line into two numbers
  left, right = line.split.map(&.to_i)
  # Append the numbers to their respective lists
  left_list << left
  right_list << right
end

# Count occurrences of each number in the right list
right_counts = Hash(Int32, Int32).new(0)
right_list.each do |num|
  right_counts[num] += 1
end

# Calculate the similarity score
similarity_score = 0
left_list.each do |num|
  similarity_score += num * right_counts[num]
end

# Output the similarity score
puts "Similarity score: #{similarity_score}"
