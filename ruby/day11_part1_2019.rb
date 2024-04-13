class Intcode
  attr_accessor :memory, :ip, :input, :output, :halted

  def initialize(program)
    @memory = program.dup
    @ip = 0
    @input = []
    @output = []
    @halted = false
  end

  def add_input(input)
    @input << input
  end

  def run
    @output = []
    while true
      opcode = @memory[@ip] % 100
      case opcode
      when 1, 2, 7, 8
        ensure_memory(@ip + 3)
        params = get_params(3)
        val1, val2 = read_memory(params[0]), read_memory(params[1])
        if opcode == 1
          write_memory(params[2], val1 + val2)
        elsif opcode == 2
          write_memory(params[2], val1 * val2)
        elsif opcode == 7
          write_memory(params[2], val1 < val2 ? 1 : 0)
        elsif opcode == 8
          write_memory(params[2], val1 == val2 ? 1 : 0)
        end
        @ip += 4
      when 3, 4
        ensure_memory(@ip + 1)
        params = get_params(1)
        if opcode == 3
          return if @input.empty?
          write_memory(params[0], @input.shift)
        else
          @output << read_memory(params[0])
        end
        @ip += 2
      when 5, 6
        ensure_memory(@ip + 2)
        params = get_params(2)
        val, target = read_memory(params[0]), read_memory(params[1])
        if (opcode == 5 && val != 0) || (opcode == 6 && val == 0)
          @ip = target
        else
          @ip += 3
        end
      when 99
        @halted = true
        return
      else
        raise "unknown opcode: #{opcode}"
      end
    end
  end

  def read_memory(address)
    ensure_memory(address)
    @memory[address]
  end

  def write_memory(address, value)
    ensure_memory(address)
    @memory[address] = value
  end

  def ensure_memory(address)
    if address >= @memory.size
      @memory += Array.new(address + 1 - @memory.size, 0)
    end
  end

  def get_params(count)
    param_modes = @memory[@ip] / 100
    params = []
    (1..count).each do |i|
      params << (@memory[@ip + i] if param_modes % 10 == 0)
      params[-1] = @ip + i if param_modes % 10 == 1
      param_modes /= 10
    end
    params
  end
end

class Robot
  attr_accessor :x, :y, :direction

  def initialize
    @x, @y = 0, 0
    @direction = 0
  end

  def turn_and_move(turn_direction)
    @direction = (@direction + (turn_direction == 0 ? 3 : 1)) % 4
    case @direction
    when 0
      @y -= 1
    when 1
      @x += 1
    when 2
      @y += 1
    when 3
      @x -= 1
    end
  end
end

data = File.read("input.txt").strip
program = data.split(",").map(&:to_i)

grid = {}
robot = Robot.new
intcode = Intcode.new(program)

until intcode.halted
  current_color = grid[[robot.x, robot.y]] || 0
  intcode.add_input(current_color)
  intcode.run
  outputs = intcode.output

  if outputs.size == 2
    grid[[robot.x, robot.y]] = outputs[0]
    robot.turn_and_move(outputs[1])
  end
end

puts grid.size