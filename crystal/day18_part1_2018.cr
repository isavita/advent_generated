# Read input from "input.txt"
input = File.read("input.txt").strip.split("\n")

# Define the initial state
state = input.map { |line| line.chars }

# Define the transition function
def next_state(state)
  new_state = Array.new(state.size) { Array.new(state[0].size, '.') }
  state.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      adjacent_trees = 0
      adjacent_lumberyards = 0
      (-1..1).each do |dy|
        (-1..1).each do |dx|
          next if dy == 0 && dx == 0
          nx, ny = x + dx, y + dy
          next if nx < 0 || nx >= row.size || ny < 0 || ny >= state.size
          case state[ny][nx]
          when '|'
            adjacent_trees += 1
          when '#'
            adjacent_lumberyards += 1
          end
        end
      end
      case cell
      when '.'
        new_state[y][x] = adjacent_trees >= 3 ? '|' : '.'
      when '|'
        new_state[y][x] = adjacent_lumberyards >= 3 ? '#' : '|'
      when '#'
        new_state[y][x] = adjacent_lumberyards >= 1 && adjacent_trees >= 1 ? '#' : '.'
      end
    end
  end
  new_state
end

# Simulate the process for 10 minutes
10.times { state = next_state(state) }

# Calculate the total resource value
wooded_acres = state.flatten.count('|')
lumberyards = state.flatten.count('#')
puts "Total resource value: #{wooded_acres * lumberyards}"