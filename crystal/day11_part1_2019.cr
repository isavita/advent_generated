
# frozen_string_literal: true

class Intcode
  getter halted : Bool

  def initialize(program : Array(Int64))
    @memory = program.dup
    @ip = 0
    @input = [] of Int64
    @output = [] of Int64
    @halted = false
  end

  def add_input(value : Int64) : Nil
    @input << value
  end

  def outputs : Array(Int64)
    @output
  end

  def run : Nil
    @output.clear
    loop do
      opcode = @memory[@ip] % 100
      case opcode
      when 1, 2, 7, 8
        ensure_memory(@ip + 3)
        params = get_params(3)
        v1 = read_memory(params[0])
        v2 = read_memory(params[1])
        case opcode
        when 1
          write_memory(params[2], v1 + v2)
        when 2
          write_memory(params[2], v1 * v2)
        when 7
          write_memory(params[2], v1 < v2 ? 1_i64 : 0_i64)
        when 8
          write_memory(params[2], v1 == v2 ? 1_i64 : 0_i64)
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
        val = read_memory(params[0])
        target = read_memory(params[1])
        if (opcode == 5 && val != 0) || (opcode == 6 && val == 0)
          @ip = target.to_i
        else
          @ip += 3
        end
      when 99
        @halted = true
        return
      else
        raise "Unknown opcode: #{opcode}"
      end
    end
  end

  private def read_memory(address : Int64) : Int64
    ensure_memory(address)
    @memory[address.to_i]
  end

  private def write_memory(address : Int64, value : Int64) : Nil
    ensure_memory(address)
    @memory[address.to_i] = value
  end

  private def ensure_memory(address : Int64) : Nil
    idx = address.to_i
    if idx >= @memory.size
      @memory.concat(Array.new(idx + 1 - @memory.size, 0_i64))
    end
  end

  private def get_params(count : Int32) : Array(Int64)
    modes = @memory[@ip] // 100
    params = [] of Int64
    (0...count).each do |i|
      mode = modes % 10
      param = @memory[@ip + i + 1]
      if mode == 1
        params << ( @ip + i + 1 ).to_i64
      else
        params << param
      end
      modes //= 10
    end
    params
  end
end

struct Robot
  property x : Int32 = 0
  property y : Int32 = 0
  property dir : Int32 = 0 # 0: up, 1: right, 2: down, 3: left

  def turn_and_move(turn : Int64) : Nil
    if turn == 0
      @dir = (@dir - 1) % 4
    else
      @dir = (@dir + 1) % 4
    end

    case @dir
    when 0 then @y -= 1
    when 1 then @x += 1
    when 2 then @y += 1
    when 3 then @x -= 1
    end
  end

  def position : Tuple(Int32, Int32)
    {@x, @y}
  end
end

def main : Nil
  program = File.read("input.txt")
                .strip
                .split(',')
                .map(&.to_i64)

  grid = Hash(Tuple(Int32, Int32), Int64).new
  robot = Robot.new
  intcode = Intcode.new(program)

  until intcode.halted
    current_color = grid.fetch(robot.position, 0_i64)
    intcode.add_input(current_color)
    intcode.run
    outs = intcode.outputs
    if outs.size == 2
      grid[robot.position] = outs[0]
      robot.turn_and_move(outs[1])
    end
  end

  puts grid.size
end

main
