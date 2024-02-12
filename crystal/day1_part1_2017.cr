
data = File.read("input.txt").chomp
sum = 0

(0...data.size).each do |i|
  next_index = (i + 1) % data.size
  if data[i] == data[next_index]
    sum += data[i].to_i
  end
end

puts sum
