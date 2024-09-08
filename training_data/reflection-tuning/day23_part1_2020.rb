# Read input from file
input = File.read('input.txt').strip.chars.map(&:to_i)

# Initialize the circular linked list
cups = Array.new(input.size + 1, 0)
input.each_with_index do |cup, i|
  cups[cup] = input[(i + 1) % input.size]
end

current = input.first
max_cup = input.size

100.times do
  # Pick up three cups
  pickup = [cups[current]]
  2.times { pickup << cups[pickup.last] }

  # Find destination
  destination = current - 1
  destination = max_cup if destination == 0
  while pickup.include?(destination)
    destination -= 1
    destination = max_cup if destination == 0
  end

  # Place picked up cups after destination
  cups[current] = cups[pickup.last]
  cups[pickup.last] = cups[destination]
  cups[destination] = pickup.first

  # Move to next cup
  current = cups[current]
end

# Generate result string
result = ""
next_cup = cups[1]
while next_cup != 1
  result += next_cup.to_s
  next_cup = cups[next_cup]
end

puts result
