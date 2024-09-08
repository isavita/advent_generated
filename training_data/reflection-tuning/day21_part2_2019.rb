class IntcodeComputer
  def initialize(program)
    @memory = program.split(',').map(&:to_i)
    @ip = 0
    @relative_base = 0
  end

  def run(input)
    output = []
    input_index = 0

    loop do
      opcode = @memory[@ip] % 100
      modes = [@memory[@ip] / 100 % 10, @memory[@ip] / 1000 % 10, @memory[@ip] / 10000 % 10]

      case opcode
      when 1 # add
        set_value(3, get_value(1, modes[0]) + get_value(2, modes[1]), modes[2])
        @ip += 4
      when 2 # multiply
        set_value(3, get_value(1, modes[0]) * get_value(2, modes[1]), modes[2])
        @ip += 4
      when 3 # input
        set_value(1, input[input_index].ord, modes[0])
        input_index += 1
        @ip += 2
      when 4 # output
        output << get_value(1, modes[0])
        @ip += 2
      when 5 # jump-if-true
        @ip = get_value(1, modes[0]) != 0 ? get_value(2, modes[1]) : @ip + 3
      when 6 # jump-if-false
        @ip = get_value(1, modes[0]) == 0 ? get_value(2, modes[1]) : @ip + 3
      when 7 # less than
        set_value(3, get_value(1, modes[0]) < get_value(2, modes[1]) ? 1 : 0, modes[2])
        @ip += 4
      when 8 # equals
        set_value(3, get_value(1, modes[0]) == get_value(2, modes[1]) ? 1 : 0, modes[2])
        @ip += 4
      when 9 # adjust relative base
        @relative_base += get_value(1, modes[0])
        @ip += 2
      when 99 # halt
        break
      else
        raise "Unknown opcode: #{opcode}"
      end
    end

    output
  end

  private

  def get_value(offset, mode)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] || 0
    when 1 then @memory[@ip + offset] || 0
    when 2 then @memory[(@memory[@ip + offset] || 0) + @relative_base] || 0
    end
  end

  def set_value(offset, value, mode)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] = value
    when 2 then @memory[(@memory[@ip + offset] || 0) + @relative_base] = value
    end
  end
end

def solve_springdroid(program, instructions)
  computer = IntcodeComputer.new(program)
  input = instructions.join("\n") + "\n"
  output = computer.run(input)
  output.last > 255 ? output.last : output.map(&:chr).join
end

program = File.read('input.txt').strip

# Part 1
instructions1 = [
  'NOT A J',
  'NOT B T',
  'OR T J',
  'NOT C T',
  'OR T J',
  'AND D J',
  'WALK'
]

result1 = solve_springdroid(program, instructions1)
puts "Part 1: #{result1}"

# Part 2
instructions2 = [
  'NOT A J',
  'NOT B T',
  'OR T J',
  'NOT C T',
  'OR T J',
  'AND D J',
  'NOT E T',
  'NOT T T',
  'OR H T',
  'AND T J',
  'RUN'
]

result2 = solve_springdroid(program, instructions2)
puts "Part 2: #{result2}"
