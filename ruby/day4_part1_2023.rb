def calculate_card_points(card)
  winning_numbers, your_numbers = card.split('|').map { |part| part.scan(/\d+/).map(&:to_i) }
  winning_set = Set.new(winning_numbers)
  
  matches = your_numbers.count { |num| winning_set.include?(num) }
  
  matches > 0 ? 2**(matches - 1) : 0
end

def calculate_total_points(input)
  input.lines.sum do |line|
    calculate_card_points(line.split(':')[1])
  end
end

# Read input from file
input = File.read('input.txt')

# Calculate and print the result
total_points = calculate_total_points(input)
puts "Total points: #{total_points}"
