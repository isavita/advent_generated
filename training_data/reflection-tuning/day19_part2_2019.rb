class IntcodeComputer
  def initialize(program)
    @memory = program.split(',').map(&:to_i)
    @ip = 0
    @relative_base = 0
  end

  def run(inputs)
    input_index = 0
    loop do
      opcode = @memory[@ip] % 100
      modes = [100, 1000, 10000].map { |i| @memory[@ip] / i % 10 }

      case opcode
      when 1 then @memory[addr(3, modes[2])] = val(1, modes[0]) + val(2, modes[1])
      when 2 then @memory[addr(3, modes[2])] = val(1, modes[0]) * val(2, modes[1])
      when 3 then @memory[addr(1, modes[0])] = inputs[input_index]; input_index += 1
      when 4 then return val(1, modes[0])
      when 5 then @ip = val(2, modes[1]) - 3 if val(1, modes[0]) != 0
      when 6 then @ip = val(2, modes[1]) - 3 if val(1, modes[0]) == 0
      when 7 then @memory[addr(3, modes[2])] = val(1, modes[0]) < val(2, modes[1]) ? 1 : 0
      when 8 then @memory[addr(3, modes[2])] = val(1, modes[0]) == val(2, modes[1]) ? 1 : 0
      when 9 then @relative_base += val(1, modes[0])
      when 99 then break
      end
      @ip += [1, 2, 3, 4, 5, 6, 7, 8, 9].include?(opcode) ? [0, 4, 4, 2, 2, 3, 3, 4, 4, 2][opcode] : 0
    end
  end

  private

  def val(offset, mode)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] || 0
    when 1 then @memory[@ip + offset] || 0
    when 2 then @memory[(@memory[@ip + offset] || 0) + @relative_base] || 0
    end
  end

  def addr(offset, mode)
    case mode
    when 0 then @memory[@ip + offset] || 0
    when 2 then (@memory[@ip + offset] || 0) + @relative_base
    end
  end
end

program = File.read('input.txt').strip

def check_point(x, y, program)
  IntcodeComputer.new(program).run([x, y]) == 1
end

# Part 1
affected = (0...50).sum { |y| (0...50).count { |x| check_point(x, y, program) } }
puts "Part 1: #{affected}"

# Part 2
x, y = 0, 100
loop do
  x += 1 until check_point(x, y, program)
  if check_point(x + 99, y - 99, program)
    puts "Part 2: #{x * 10000 + (y - 99)}"
    break
  end
  y += 1
end
