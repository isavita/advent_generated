require 'set'

class IntcodeComputer
  # ... (implementation of IntcodeComputer class)
end

def find_oxygen_system(program)
  computer = IntcodeComputer.new(program)
  start = [0, 0]
  queue = [[start, 0]]  # [position, steps]
  visited = Set.new([start])
  directions = [[0, 1], [0, -1], [-1, 0], [1, 0]]
  move_commands = [1, 2, 3, 4]

  while !queue.empty?
    pos, steps = queue.shift

    directions.each_with_index do |(dx, dy), i|
      new_pos = [pos[0] + dx, pos[1] + dy]
      next if visited.include?(new_pos)

      computer.reset
      path_to_pos(start, pos).each { |move| computer.run_until_input_or_halt(move) }
      output = computer.run_until_input_or_halt(move_commands[i])

      case output
      when 0  # Wall
        next
      when 2  # Oxygen system found
        return steps + 1
      when 1  # Open space
        visited.add(new_pos)
        queue.push([new_pos, steps + 1])
      end
    end
  end

  -1  # Oxygen system not found
end

def path_to_pos(start, target)
  dx = target[0] - start[0]
  dy = target[1] - start[1]
  path = []
  path += [3] * -dx if dx < 0
  path += [4] * dx if dx > 0
  path += [2] * -dy if dy < 0
  path += [1] * dy if dy > 0
  path
end

# Read the input and run the solution
program = File.read('input.txt').split(',').map(&:to_i)
puts find_oxygen_system(program)
