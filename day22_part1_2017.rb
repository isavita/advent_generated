
input = File.read('input.txt').split("\n")

grid = Hash.new('.')
input.each_with_index do |row, y|
  row.chars.each_with_index do |char, x|
    grid[[x, y]] = '#' if char == '#'
  end
end

directions = [[0, -1], [1, 0], [0, 1], [-1, 0]]
current_direction = 0
current_position = [input.size / 2, input.size / 2]
infections = 0

10000.times do
  if grid[current_position] == '#'
    current_direction = (current_direction + 1) % 4
    grid[current_position] = '.'
  else
    current_direction = (current_direction - 1) % 4
    grid[current_position] = '#'
    infections += 1
  end

  current_position = [current_position[0] + directions[current_direction][0], current_position[1] + directions[current_direction][1]]
end

puts infections
