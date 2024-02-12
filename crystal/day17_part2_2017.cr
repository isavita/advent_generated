
data = File.read("input.txt").chomp.to_i
current_pos = 0
value_after_zero = 0

(1..50000000).each do |i|
  current_pos = (current_pos + data) % i
  value_after_zero = i if current_pos == 0
  current_pos += 1
end

puts value_after_zero
