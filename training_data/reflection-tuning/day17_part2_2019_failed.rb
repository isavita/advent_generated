class IntcodeComputer
  def initialize(program)
    @memory = program.split(',').map(&:to_i)
    @pointer = 0
    @relative_base = 0
    @inputs = []
    @outputs = []
  end

  def run
    loop do
      opcode = @memory[@pointer] % 100
      modes = [@memory[@pointer] / 100 % 10, @memory[@pointer] / 1000 % 10, @memory[@pointer] / 10000 % 10]

      case opcode
      when 1 then add(modes)
      when 2 then multiply(modes)
      when 3 then input(modes)
      when 4 then output(modes)
      when 5 then jump_if_true(modes)
      when 6 then jump_if_false(modes)
      when 7 then less_than(modes)
      when 8 then equals(modes)
      when 9 then adjust_relative_base(modes)
      when 99 then break
      else
        raise "Unknown opcode: #{opcode}"
      end
    end
  end

  def add_input(value)
    @inputs << value
  end

  def get_output
    @outputs.shift
  end

  private

  # Implement the necessary Intcode operations (add, multiply, input, output, etc.)
  # ...

end

def calculate_alignment_parameters(scaffold_map)
  rows = scaffold_map.split("\n")
  height = rows.size
  width = rows[0].size
  intersections = []

  (1...height-1).each do |y|
    (1...width-1).each do |x|
      if rows[y][x] == '#' &&
         rows[y-1][x] == '#' &&
         rows[y+1][x] == '#' &&
         rows[y][x-1] == '#' &&
         rows[y][x+1] == '#'
        intersections << [x, y]
      end
    end
  end

  intersections.sum { |x, y| x * y }
end

# Part One
input = File.read('input.txt').strip
computer = IntcodeComputer.new(input)
computer.run

scaffold_map = ''
while (output = computer.get_output)
  scaffold_map += output.chr
end

puts "Part One: #{calculate_alignment_parameters(scaffold_map)}"

# Part Two
computer = IntcodeComputer.new(input)
computer.memory[0] = 2  # Wake up the vacuum robot

main_routine = "A,B,A,C,A,B,C,B,C,B\n"
function_a = "L,10,R,8,R,6,R,10\n"
function_b = "L,10,L,12,R,8,R,8\n"
function_c = "R,4,R,6,R,6,R,4,R,4\n"
video_feed = "n\n"

[main_routine, function_a, function_b, function_c, video_feed].each do |input_string|
  input_string.each_char { |c| computer.add_input(c.ord) }
end

computer.run

dust_collected = 0
while (output = computer.get_output)
  if output > 255
    dust_collected = output
    break
  end
end

puts "Part Two: #{dust_collected}"
