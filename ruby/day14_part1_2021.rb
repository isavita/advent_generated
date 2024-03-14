require 'set'

# Read input from file
input = File.read('input.txt').split("\n")

# Parse input
template = input.first
rules = Hash[input[2..-1].map { |line| line.split(' -> ') }]

# Perform 10 steps of pair insertion
10.times do
  new_template = template[0]
  (0...template.length-1).each do |i|
    pair = template[i,2]
    new_template << rules[pair] << pair[1]
  end
  template = new_template
end

# Count element frequencies
counts = Hash.new(0)
template.each_char { |c| counts[c] += 1 }

# Find most and least common elements
most_common = counts.max_by { |_, v| v }[1]
least_common = counts.min_by { |_, v| v }[1]

# Print the answer
puts most_common - least_common