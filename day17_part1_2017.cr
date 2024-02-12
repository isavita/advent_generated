
data = File.read("input.txt").chomp.to_i
buffer = [0]
current_pos = 0

(1..2017).each do |i|
  current_pos = (current_pos + data) % buffer.size
  buffer.insert(current_pos + 1, i)
  current_pos += 1
end

index = buffer.index(2017)
if index
  puts buffer[(index + 1) % buffer.size]
else
  puts "Value not found in buffer"
end
