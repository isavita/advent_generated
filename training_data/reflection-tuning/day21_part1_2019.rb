# Read the Intcode program from the input file
intcode = File.read('input.txt').strip.split(',').map(&:to_i)

# Define the Intcode computer
class IntcodeComputer
  def initialize(program)
    @memory = program.dup
    @ip = 0
    @relative_base = 0
  end

  def run(input)
    output = []
    input_index = 0

    loop do
      opcode = @memory[@ip] % 100
      modes = [100, 1000, 10000].map { |i| @memory[@ip] / i % 10 }

      case opcode
      when 1 then @memory[addr(3, modes[2])] = val(1, modes[0]) + val(2, modes[1])
      when 2 then @memory[addr(3, modes[2])] = val(1, modes[0]) * val(2, modes[1])
      when 3 then @memory[addr(1, modes[0])] = input[input_index]; input_index += 1
      when 4 then output << val(1, modes[0])
      when 5 then @ip = val(2, modes[1]) - 3 if val(1, modes[0]) != 0
      when 6 then @ip = val(2, modes[1]) - 3 if val(1, modes[0]) == 0
      when 7 then @memory[addr(3, modes[2])] = val(1, modes[0]) < val(2, modes[1]) ? 1 : 0
      when 8 then @memory[addr(3, modes[2])] = val(1, modes[0]) == val(2, modes[1]) ? 1 : 0
      when 9 then @relative_base += val(1, modes[0])
      when 99 then break
      end
      @ip += [1, 2, 3, 4, 5, 6, 7, 8, 9].include?(opcode) ? [0, 4, 4, 2, 2, 3, 3, 4, 4, 2][opcode] : 0
    end
    output
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
    mode == 2 ? (@memory[@ip + offset] || 0) + @relative_base : (@memory[@ip + offset] || 0)
  end
end

# Create the springscript program
springscript = <<~SCRIPT
  NOT A T
  NOT B J
  OR T J
  NOT C T
  OR T J
  AND D J
  WALK
SCRIPT

# Convert the springscript to ASCII
input = springscript.chars.map(&:ord)

# Run the Intcode program
computer = IntcodeComputer.new(intcode)
output = computer.run(input)

# Interpret the output
if output.last > 255
  puts output.last
else
  puts output.map(&:chr).join
end
