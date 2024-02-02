
horizontal_position = 0
depth = 0
aim = 0

File.open("input.txt").each do |line|
  action, value = line.split
  value = value.to_i

  case action
  when "forward"
    horizontal_position += value
    depth += aim * value
  when "down"
    aim += value
  when "up"
    aim -= value
  end
end

puts horizontal_position * depth
