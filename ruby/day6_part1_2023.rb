def parse_input(input)
  lines = input.split("\n")
  times = lines[0].split(':')[1].split.map(&:to_i)
  distances = lines[1].split(':')[1].split.map(&:to_i)
  times.zip(distances)
end

def count_winning_strategies(time, record_distance)
  winning_count = 0
  (0..time).each do |hold_time|
    distance = hold_time * (time - hold_time)
    winning_count += 1 if distance > record_distance
  end
  winning_count
end

def calculate_margin_of_error(races)
  races.map { |time, distance| count_winning_strategies(time, distance) }.reduce(:*)
end

# Read input from file
input = File.read('input.txt')

# Parse input and calculate result
races = parse_input(input)
result = calculate_margin_of_error(races)

puts "The margin of error is: #{result}"
