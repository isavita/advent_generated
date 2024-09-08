class IntcodeComputer
  def initialize(program)
    @memory = program.split(',').map(&:to_i)
    @ip = 0
    @relative_base = 0
  end

  def run(input)
    loop do
      opcode = @memory[@ip] % 100
      modes = [100, 1000, 10000].map { |i| @memory[@ip] / i % 10 }

      case opcode
      when 1 then add(modes)
      when 2 then multiply(modes)
      when 3 then store(modes, input)
      when 4 then return output(modes)
      when 5 then jump_if_true(modes)
      when 6 then jump_if_false(modes)
      when 7 then less_than(modes)
      when 8 then equals(modes)
      when 9 then adjust_relative_base(modes)
      when 99 then break
      end
    end
  end

  private

  def add(modes)
    @memory[address(3, modes[2])] = value(1, modes[0]) + value(2, modes[1])
    @ip += 4
  end

  def multiply(modes)
    @memory[address(3, modes[2])] = value(1, modes[0]) * value(2, modes[1])
    @ip += 4
  end

  def store(modes, input)
    @memory[address(1, modes[0])] = input
    @ip += 2
  end

  def output(modes)
    result = value(1, modes[0])
    @ip += 2
    result
  end

  def jump_if_true(modes)
    @ip = value(1, modes[0]) != 0 ? value(2, modes[1]) : @ip + 3
  end

  def jump_if_false(modes)
    @ip = value(1, modes[0]) == 0 ? value(2, modes[1]) : @ip + 3
  end

  def less_than(modes)
    @memory[address(3, modes[2])] = value(1, modes[0]) < value(2, modes[1]) ? 1 : 0
    @ip += 4
  end

  def equals(modes)
    @memory[address(3, modes[2])] = value(1, modes[0]) == value(2, modes[1]) ? 1 : 0
    @ip += 4
  end

  def adjust_relative_base(modes)
    @relative_base += value(1, modes[0])
    @ip += 2
  end

  def value(offset, mode)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] || 0
    when 1 then @memory[@ip + offset] || 0
    when 2 then @memory[(@memory[@ip + offset] || 0) + @relative_base] || 0
    end
  end

  def address(offset, mode)
    case mode
    when 0, 1 then @memory[@ip + offset] || 0
    when 2 then (@memory[@ip + offset] || 0) + @relative_base
    end
  end
end

def explore_map(program)
  droid = IntcodeComputer.new(program)
  map = { [0, 0] => '.' }
  queue = [[0, 0, 0]]
  visited = Set.new([[0, 0]])
  oxygen_pos = nil

  until queue.empty?
    x, y, steps = queue.shift
    [[1, 0], [0, 1], [-1, 0], [0, -1]].each_with_index do |(dx, dy), direction|
      nx, ny = x + dx, y + dy
      next if visited.include?([nx, ny])

      status = droid.run(direction + 1)
      visited.add([nx, ny])

      case status
      when 0
        map[[nx, ny]] = '#'
      when 1
        map[[nx, ny]] = '.'
        queue << [nx, ny, steps + 1]
      when 2
        map[[nx, ny]] = 'O'
        oxygen_pos = [nx, ny]
        return [map, steps + 1, oxygen_pos]
      end

      droid.run((direction + 2) % 4 + 1) if status != 0
    end
  end
end

def fill_with_oxygen(map, start)
  queue = [start]
  minutes = 0

  until queue.empty?
    new_queue = []
    queue.each do |x, y|
      [[1, 0], [0, 1], [-1, 0], [0, -1]].each do |dx, dy|
        nx, ny = x + dx, y + dy
        if map[[nx, ny]] == '.'
          map[[nx, ny]] = 'O'
          new_queue << [nx, ny]
        end
      end
    end
    queue = new_queue
    minutes += 1 unless queue.empty?
  end

  minutes
end

program = File.read('input.txt').strip
map, steps_to_oxygen, oxygen_pos = explore_map(program)
minutes_to_fill = fill_with_oxygen(map, oxygen_pos)

puts "Part 1: #{steps_to_oxygen}"
puts "Part 2: #{minutes_to_fill}"
