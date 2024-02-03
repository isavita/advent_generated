
data = File.readlines('input.txt').map(&:to_i)

larger_than_previous = 0
data.each_with_index do |depth, index|
  next if index == 0

  if depth > data[index - 1]
    larger_than_previous += 1
  end
end

puts larger_than_previous

sums = data.each_cons(3).map { |a, b, c| a + b + c }

larger_than_previous_sum = 0
sums.each_with_index do |sum, index|
  next if index == 0

  if sum > sums[index - 1]
    larger_than_previous_sum += 1
  end
end

puts larger_than_previous_sum
