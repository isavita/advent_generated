# Read and parse input
grid = File.readlines('input.txt').map(&:strip)
infected = {}
center_y, center_x = grid.size / 2, grid[0].size / 2

grid.each_with_index do |row, y|
  row.chars.each_with_index do |cell, x|
    infected[[y - center_y, x - center_x]] = true if cell == '#'
  end
end

# Initialize virus carrier
pos = [0, 0]
dir = [-1, 0]  # Facing up
infections = 0

# Simulate 10000 bursts
10000.times do
  if infected[pos]
    dir = [dir[1], -dir[0]]  # Turn right
    infected.delete(pos)
  else
    dir = [-dir[1], dir[0]]  # Turn left
    infected[pos] = true
    infections += 1
  end
  pos = [pos[0] + dir[0], pos[1] + dir[1]]  # Move forward
end

puts infections
