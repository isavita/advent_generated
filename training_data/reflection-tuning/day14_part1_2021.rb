# Read and parse input
input = File.read('input.txt').split("\n\n")
template = input[0]
rules = input[1].split("\n").map { |rule| rule.split(' -> ') }.to_h

# Initialize pair counts
pair_counts = Hash.new(0)
template.chars.each_cons(2) { |pair| pair_counts[pair.join] += 1 }

# Perform pair insertion for 10 steps
10.times do
  new_counts = Hash.new(0)
  pair_counts.each do |pair, count|
    insert = rules[pair]
    new_counts[pair[0] + insert] += count
    new_counts[insert + pair[1]] += count
  end
  pair_counts = new_counts
end

# Count elements
element_counts = Hash.new(0)
pair_counts.each do |pair, count|
  element_counts[pair[0]] += count
end
element_counts[template[-1]] += 1  # Add last element of original template

# Calculate result
min, max = element_counts.values.minmax
puts max - min
