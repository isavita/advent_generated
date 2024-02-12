
file = File.open("input.txt")
horizontal_position = 0
depth = 0
aim = 0

file.each_line do |line|
  command = line.split(" ")
  direction = command[0]
  units = command[1].to_i

  case direction
  when "forward"
    horizontal_position += units
    depth += aim * units
  when "down"
    aim += units
  when "up"
    aim -= units
  end
end

product = horizontal_position * depth
puts product
