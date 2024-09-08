require 'set'
require 'algorithms'

def shortest_path(grid)
  start_positions = []
  keys = {}
  doors = {}

  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      case cell
      when '@'
        start_positions << [y, x]
      when /[a-z]/
        keys[cell] = [y, x]
      when /[A-Z]/
        doors[cell.downcase] = [y, x]
      end
    end
  end

  all_keys = keys.keys.to_set
  initial_state = [start_positions, Set.new]
  queue = Containers::PriorityQueue.new
  queue.push(initial_state, 0)
  visited = Set.new

  while !queue.empty?
    state, steps = queue.pop
    positions, collected_keys = state

    return steps if collected_keys == all_keys

    state_key = [positions, collected_keys].hash
    next if visited.include?(state_key)
    visited.add(state_key)

    positions.each_with_index do |pos, robot_index|
      y, x = pos
      [[y-1, x], [y+1, x], [y, x-1], [y, x+1]].each do |ny, nx|
        next if ny < 0 || ny >= grid.size || nx < 0 || nx >= grid[0].size
        cell = grid[ny][nx]
        next if cell == '#'
        if cell =~ /[A-Z]/ && !collected_keys.include?(cell.downcase)
          next
        end

        new_positions = positions.dup
        new_positions[robot_index] = [ny, nx]
        new_keys = collected_keys.dup

        if cell =~ /[a-z]/
          new_keys.add(cell)
        end

        new_state = [new_positions, new_keys]
        new_steps = steps + 1
        queue.push(new_state, -new_steps)
      end
    end
  end

  -1  # No solution found
end

# Read input
grid = File.readlines('input.txt').map(&:chomp).map(&:chars)

# Modify the center of the grid for part 2
center_y, center_x = grid.size / 2, grid[0].size / 2
[[-1, -1], [-1, 1], [1, -1], [1, 1]].each do |dy, dx|
  grid[center_y + dy][center_x + dx] = '@'
end
grid[center_y][center_x] = '#'
grid[center_y - 1][center_x] = '#'
grid[center_y + 1][center_x] = '#'
grid[center_y][center_x - 1] = '#'
grid[center_y][center_x + 1] = '#'

puts shortest_path(grid)
