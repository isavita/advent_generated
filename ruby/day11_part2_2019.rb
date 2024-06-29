class IntcodeComputer
  def initialize(program)
    @memory = program.split(',').map(&:to_i)
    @ip = 0
    @relative_base = 0
    @halted = false
  end

  def run(input)
    outputs = []
    loop do
      opcode = @memory[@ip] % 100
      modes = [100, 1000, 10000].map { |i| @memory[@ip] / i % 10 }
      
      case opcode
      when 1 # Add
        set(3, get(1, modes[0]) + get(2, modes[1]), modes[2])
        @ip += 4
      when 2 # Multiply
        set(3, get(1, modes[0]) * get(2, modes[1]), modes[2])
        @ip += 4
      when 3 # Input
        return :need_input if input.nil?
        set(1, input, modes[0])
        input = nil
        @ip += 2
      when 4 # Output
        outputs << get(1, modes[0])
        @ip += 2
        return outputs if outputs.size == 2
      when 5 # Jump-if-true
        @ip = get(1, modes[0]) != 0 ? get(2, modes[1]) : @ip + 3
      when 6 # Jump-if-false
        @ip = get(1, modes[0]) == 0 ? get(2, modes[1]) : @ip + 3
      when 7 # Less than
        set(3, get(1, modes[0]) < get(2, modes[1]) ? 1 : 0, modes[2])
        @ip += 4
      when 8 # Equals
        set(3, get(1, modes[0]) == get(2, modes[1]) ? 1 : 0, modes[2])
        @ip += 4
      when 9 # Adjust relative base
        @relative_base += get(1, modes[0])
        @ip += 2
      when 99 # Halt
        @halted = true
        return :halted
      else
        raise "Unknown opcode: #{opcode}"
      end
    end
  end

  def halted?
    @halted
  end

  private

  def get(param, mode)
    case mode
    when 0 then @memory[@memory[@ip + param]] || 0
    when 1 then @memory[@ip + param] || 0
    when 2 then @memory[@relative_base + (@memory[@ip + param] || 0)] || 0
    else raise "Unknown mode: #{mode}"
    end
  end

  def set(param, value, mode)
    case mode
    when 0 then @memory[@memory[@ip + param]] = value
    when 2 then @memory[@relative_base + @memory[@ip + param]] = value
    else raise "Invalid mode for setting: #{mode}"
    end
  end
end

class HullPainter
  def initialize(program)
    @computer = IntcodeComputer.new(program)
    @hull = Hash.new(0)
    @position = [0, 0]
    @direction = [0, 1]
  end

  def paint(starting_color = 0)
    @hull[@position] = starting_color
    
    until @computer.halted?
      color = @hull[@position]
      output = @computer.run(color)
      break if output == :halted

      @hull[@position] = output[0]
      turn(output[1])
      move
    end
  end

  def painted_panels_count
    @hull.size
  end

  def print_hull
    x_range = @hull.keys.map(&:first).minmax
    y_range = @hull.keys.map(&:last).minmax

    y_range[1].downto(y_range[0]).each do |y|
      x_range[0].upto(x_range[1]).each do |x|
        print @hull[[x, y]] == 1 ? '█' : ' '
      end
      puts
    end
  end

  private

  def turn(direction)
    @direction = direction == 0 ? [-@direction[1], @direction[0]] : [@direction[1], -@direction[0]]
  end

  def move
    @position = [@position[0] + @direction[0], @position[1] + @direction[1]]
  end
end

# Read input
program = File.read('input.txt').strip

# Part 1
painter = HullPainter.new(program)
painter.paint
puts "Part 1: Number of panels painted at least once: #{painter.painted_panels_count}"

# Part 2
painter = HullPainter.new(program)
painter.paint(1)
puts "\nPart 2: Registration identifier:"
painter.print_hull
