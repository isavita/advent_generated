
horizontal_position = 0
depth = 0

File.open("input.txt").each do |line|
  action, value = line.split
  value = value.to_i

  case action
  when "forward"
    horizontal_position += value
  when "down"
    depth += value
  when "up"
    depth -= value
  end
end

puts horizontal_position * depth
