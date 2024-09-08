require_relative 'intcode'  # Assume this file contains the Intcode computer implementation

class RepairDroid
  DIRECTIONS = [[0, -1], [0, 1], [-1, 0], [1, 0]]  # North, South, West, East
  MOVE_COMMANDS = [1, 2, 3, 4]

  def initialize(program)
    @computer = Intcode.new(program)
    @map = Hash.new(' ')
    @position = [0, 0]
    @map[@position] = '.'
    @oxygen_position = nil
  end

  def explore
    queue = [[@position, 0]]
    visited = Set.new([@position])

    until queue.empty?
      pos, steps = queue.shift
      return steps if @oxygen_position == pos

      DIRECTIONS.each_with_index do |dir, i|
        new_pos = [pos[0] + dir[0], pos[1] + dir[1]]
        next if visited.include?(new_pos)

        @computer.input.push(MOVE_COMMANDS[i])
        status = @computer.run
        case status
        when 0
          @map[new_pos] = '#'
        when 1, 2
          @map[new_pos] = '.'
          visited.add(new_pos)
          queue.push([new_pos, steps + 1])
          @oxygen_position = new_pos if status == 2
        end
        # Move back
        @computer.input.push(MOVE_COMMANDS[i ^ 1])
        @computer.run
      end
    end
  end

  def fill_oxygen
    queue = [[@oxygen_position, 0]]
    visited = Set.new([@oxygen_position])
    max_time = 0

    until queue.empty?
      pos, time = queue.shift
      max_time = [max_time, time].max

      DIRECTIONS.each do |dir|
        new_pos = [pos[0] + dir[0], pos[1] + dir[1]]
        next if visited.include?(new_pos) || @map[new_pos] != '.'

        visited.add(new_pos)
        queue.push([new_pos, time + 1])
      end
    end

    max_time
  end
end

program = File.read('input.txt').split(',').map(&:to_i)
droid = RepairDroid.new(program)

puts "Part 1: #{droid.explore}"
puts "Part 2: #{droid.fill_oxygen}"
