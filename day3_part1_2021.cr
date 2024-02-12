
file = File.open("input.txt")
counts = Array.new(12) { [0, 0] }

file.each_line do |line|
  num = line.strip
  num.each_char.with_index do |char, i|
    counts[i][char.to_i] += 1
  end
end

gamma_rate = 0
epsilon_rate = 0
counts.each_with_index do |count, i|
  if count[0] > count[1]
    gamma_rate |= 1 << (counts.size - i - 1)
  else
    epsilon_rate |= 1 << (counts.size - i - 1)
  end
end

puts gamma_rate * epsilon_rate
