
instructions = File.readlines('input.txt').map(&:chomp)

directions = [[1, 0], [0, 1], [-1, 0], [0, -1]]
current_dir = 0
x = 0
y = 0

instructions.each do |instruction|
  action = instruction[0]
  value = instruction[1..].to_i

  case action
  when 'N'
    y -= value
  when 'S'
    y += value
  when 'E'
    x += value
  when 'W'
    x -= value
  when 'L'
    current_dir = (current_dir - value / 90) % 4
  when 'R'
    current_dir = (current_dir + value / 90) % 4
  when 'F'
    x += directions[current_dir][0] * value
    y += directions[current_dir][1] * value
  end
end

puts x.abs + y.abs
