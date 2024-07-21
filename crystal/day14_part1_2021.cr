
# Read input from file
input = File.read("input.txt").split("\n")

# The first line is the polymer template
template = input[0].chomp

# The rest are the pair insertion rules
rules = Hash(String, String).new
input[2..-1].each do |line|
  pair, insert = line.split(" -> ")
  rules[pair] = insert
end

# Function to perform the polymerization for a given number of steps
def polymerize(template : String, rules : Hash(String, String), steps : Int32) : String
  current = template

  steps.times do
    new_polymer = String.build do |s|
      current.chars.each_with_index do |char, index|
        s << char
        if index < current.size - 1
          pair = "#{char}#{current[index + 1]}"
          if rules.has_key?(pair)
            s << rules[pair]
          end
        end
      end
    end
    current = new_polymer
  end

  current
end

# Perform the polymerization for 10 steps
result_polymer = polymerize(template, rules, 10)

# Count the occurrences of each element
element_counts = Hash(Char, Int32).new
result_polymer.each_char do |char|
  element_counts[char] ||= 0
  element_counts[char] += 1
end

# Find the most and least common elements
max_count = element_counts.values.max
min_count = element_counts.values.min

# Calculate the difference
difference = max_count - min_count

# Print the result
puts difference
