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
      when 1 then op_add(modes)
      when 2 then op_multiply(modes)
      when 3 then op_input(modes, input)
      when 4 then return op_output(modes)
      when 5 then op_jump_if_true(modes)
      when 6 then op_jump_if_false(modes)
      when 7 then op_less_than(modes)
      when 8 then op_equals(modes)
      when 9 then op_adjust_relative_base(modes)
      when 99 then return nil
      else raise "Unknown opcode: #{opcode}"
      end
    end
  end

  private

  def read(mode, pos)
    case mode
    when 0 then @memory[@memory[pos] || 0] || 0
    when 1 then @memory[pos] || 0
    when 2 then @memory[(@memory[pos] || 0) + @relative_base] || 0
    end
  end

  def write(mode, pos, value)
    case mode
    when 0 then @memory[@memory[pos] || 0] = value
    when 2 then @memory[(@memory[pos] || 0) + @relative_base] = value
    end
  end

  def op_add(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) + read(modes[1], @ip + 2))
    @ip += 4
  end

  def op_multiply(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) * read(modes[1], @ip + 2))
    @ip += 4
  end

  def op_input(modes, input)
    write(modes[0], @ip + 1, input)
    @ip += 2
  end

  def op_output(modes)
    output = read(modes[0], @ip + 1)
    @ip += 2
    output
  end

  def op_jump_if_true(modes)
    @ip = read(modes[0], @ip + 1) != 0 ? read(modes[1], @ip + 2) : @ip + 3
  end

  def op_jump_if_false(modes)
    @ip = read(modes[0], @ip + 1) == 0 ? read(modes[1], @ip + 2) : @ip + 3
  end

  def op_less_than(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) < read(modes[1], @ip + 2) ? 1 : 0)
    @ip += 4
  end

  def op_equals(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) == read(modes[1], @ip + 2) ? 1 : 0)
    @ip += 4
  end

  def op_adjust_relative_base(modes)
    @relative_base += read(modes[0], @ip + 1)
    @ip += 2
  end
end

def find_oxygen_system(program)
  computer = IntcodeComputer.new(program)
  queue = [[0, 0, 0]]  # [x, y, steps]
  visited = Set.new([[0, 0]])
  directions = [[0, -1], [0, 1], [-1, 0], [1, 0]]

  while !queue.empty?
    x, y, steps = queue.shift

    directions.each_with_index do |(dx, dy), i|
      nx, ny = x + dx, y + dy
      next if visited.include?([nx, ny])

      status = computer.run(i + 1)
      case status
      when 0  # Wall
        visited.add([nx, ny])
      when 1  # Moved
        visited.add([nx, ny])
        queue.push([nx, ny, steps + 1])
      when 2  # Oxygen system found
        return steps + 1
      end

      # Move back
      computer.run(i % 2 == 0 ? i + 2 : i)
    end
  end
end

program = File.read('input.txt').strip
steps = find_oxygen_system(program)
puts steps
