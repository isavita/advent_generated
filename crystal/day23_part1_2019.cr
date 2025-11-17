
require "deque"

class IntcodeComputer
  property memory : Hash(Int64, Int64)
  property ip : Int64 = 0_i64
  property relative_base : Int64 = 0_i64
  property inputs : Deque(Int64) = Deque(Int64).new
  property outputs : Array(Int64) = [] of Int64
  property halted : Bool = false
  property needs_input : Bool = false

  def initialize(program : Array(Int64), initial_inputs : Deque(Int64) = Deque(Int64).new)
    @memory = Hash(Int64, Int64).new(0_i64)
    program.each_with_index { |v, i| @memory[i.to_i64] = v }
    @inputs = initial_inputs
  end

  private def param_value(mode : Int64, offset : Int64) : Int64
    addr = @memory[@ip + offset]
    case mode
    when 0 then @memory[addr]
    when 1 then addr
    when 2 then @memory[@relative_base + addr]
    else    raise "bad mode"
    end
  end

  private def param_address(mode : Int64, offset : Int64) : Int64
    addr = @memory[@ip + offset]
    case mode
    when 0 then addr
    when 2 then @relative_base + addr
    else    raise "bad mode"
    end
  end

  def run
    loop do
      instr = @memory[@ip]
      opcode = (instr % 100).to_i
      mode1 = (instr / 100 % 10).to_i
      mode2 = (instr / 1000 % 10).to_i
      mode3 = (instr / 10000 % 10).to_i

      case opcode
      when 99
        @halted = true
        break
      when 1
        p1 = param_value(mode1, 1)
        p2 = param_value(mode2, 2)
        @memory[param_address(mode3, 3)] = p1 + p2
        @ip += 4
      when 2
        p1 = param_value(mode1, 1)
        p2 = param_value(mode2, 2)
        @memory[param_address(mode3, 3)] = p1 * p2
        @ip += 4
      when 3
        if @inputs.empty?
          @needs_input = true
          return
        end
        @needs_input = false
        @memory[param_address(mode1, 1)] = @inputs.shift
        @ip += 2
      when 4
        @outputs << param_value(mode1, 1)
        @ip += 2
        return if @outputs.size == 3
      when 5
        p1 = param_value(mode1, 1)
        p2 = param_value(mode2, 2)
        @ip = p1 != 0 ? p2 : @ip + 3
      when 6
        p1 = param_value(mode1, 1)
        p2 = param_value(mode2, 2)
        @ip = p1 == 0 ? p2 : @ip + 3
      when 7
        p1 = param_value(mode1, 1)
        p2 = param_value(mode2, 2)
        @memory[param_address(mode3, 3)] = p1 < p2 ? 1_i64 : 0_i64
        @ip += 4
      when 8
        p1 = param_value(mode1, 1)
        p2 = param_value(mode2, 2)
        @memory[param_address(mode3, 3)] = p1 == p2 ? 1_i64 : 0_i64
        @ip += 4
      when 9
        @relative_base += param_value(mode1, 1)
        @ip += 2
      else
        raise "unknown opcode #{opcode}"
      end
    end
  end
end

program = File.read("input.txt").strip.split(',').map(&.to_i64)

computers = Array(IntcodeComputer).new
50.times do |i|
  computers << IntcodeComputer.new(program, Deque(Int64){i.to_i64})
end

queues = Array(Deque({Int64, Int64})).new
50.times { queues << Deque({Int64, Int64}).new }

found = false
while true
  50.times do |i|
    comp = computers[i]
    if !queues[i].empty?
      x, y = queues[i].shift
      comp.inputs << x
      comp.inputs << y
    else
      comp.inputs << -1_i64
    end
    comp.run
    while comp.outputs.size >= 3
      dest = comp.outputs.shift
      x = comp.outputs.shift
      y = comp.outputs.shift
      if dest == 255
        puts y unless found
        found = true
        exit 0
      else
        queues[dest].push({x, y}) if dest >= 0 && dest < 50
      end
    end
  end
end
