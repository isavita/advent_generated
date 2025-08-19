
require "deque"

class IntcodeComputer
  property memory : Hash(Int64, Int64)
  property ip : Int64 = 0_i64
  property relative_base : Int64 = 0_i64
  property inputs : Deque(Int64)
  property outputs : Array(Int64) = [] of Int64
  property halted : Bool = false
  property idle : Bool = false

  def initialize(program : Array(Int64), inputs : Array(Int64) = [] of Int64)
    @memory = Hash(Int64, Int64).new(0_i64)
    program.each_with_index { |v, i| @memory[i.to_i64] = v }
    @inputs = Deque(Int64).new
    inputs.each { |v| @inputs << v }
  end

  private def get_param(mode : Int64, offset : Int64) : Int64
    case mode
    when 0_i64 then @memory[@memory[@ip + offset]]
    when 1_i64 then @memory[@ip + offset]
    when 2_i64 then @memory[@relative_base + @memory[@ip + offset]]
    else            raise "unknown mode #{mode}"
    end
  end

  private def set_param(mode : Int64, offset : Int64, value : Int64)
    case mode
    when 0_i64 then @memory[@memory[@ip + offset]] = value
    when 2_i64 then @memory[@relative_base + @memory[@ip + offset]] = value
    else            raise "unknown mode #{mode}"
    end
  end

  def run
    loop do
      opcode = @memory[@ip] % 100_i64
      modes = [
        (@memory[@ip] // 100_i64) % 10_i64,
        (@memory[@ip] // 1000_i64) % 10_i64,
        (@memory[@ip] // 10000_i64) % 10_i64
      ]

      case opcode
      when 99_i64
        @halted = true
        break
      when 1_i64, 2_i64, 7_i64, 8_i64
        p1 = get_param(modes[0], 1_i64)
        p2 = get_param(modes[1], 2_i64)
        result = case opcode
                 when 1_i64 then p1 + p2
                 when 2_i64 then p1 * p2
                 when 7_i64 then (p1 < p2) ? 1_i64 : 0_i64
                 else            (p1 == p2) ? 1_i64 : 0_i64
                 end
        set_param(modes[2], 3_i64, result)
        @ip += 4_i64
      when 3_i64
        if @inputs.empty?
          set_param(modes[0], 1_i64, -1_i64)
          @ip += 2_i64
          @idle = true
          return
        else
          value = @inputs.shift
          set_param(modes[0], 1_i64, value)
          @ip += 2_i64
          @idle = false
        end
      when 4_i64
        @outputs << get_param(modes[0], 1_i64)
        @ip += 2_i64
        @idle = false
        return if @outputs.size >= 3
      when 5_i64, 6_i64
        p1 = get_param(modes[0], 1_i64)
        p2 = get_param(modes[1], 2_i64)
        if (opcode == 5_i64 && p1 != 0_i64) || (opcode == 6_i64 && p1 == 0_i64)
          @ip = p2
        else
          @ip += 3_i64
        end
      when 9_i64
        @relative_base += get_param(modes[0], 1_i64)
        @ip += 2_i64
      else
        raise "unknown opcode #{opcode}"
      end
    end
  end
end

# ---------- main ----------
program = File.read("input.txt")
            .strip
            .split(',')
            .map(&.to_i64)

computers = [] of IntcodeComputer
50.times do |addr|
  computers << IntcodeComputer.new(program.dup, [addr.to_i64])
end

packet_queues = Array(Deque({Int64, Int64})).new(50) { Deque({Int64, Int64}).new }

nat_packet : {Int64, Int64}? = nil
prev_nat_y : Int64? = nil

loop do
  # feed inputs / run each computer
  50.times do |i|
    if !packet_queues[i].empty?
      x, y = packet_queues[i].shift
      computers[i].inputs << x
      computers[i].inputs << y
      computers[i].idle = false
    else
      computers[i].inputs << -1_i64
    end

    computers[i].run

    while computers[i].outputs.size >= 3
      dest = computers[i].outputs.shift.to_i64
      x    = computers[i].outputs.shift.to_i64
      y    = computers[i].outputs.shift.to_i64

      if dest == 255_i64
        nat_packet = {x, y}
      elsif dest >= 0_i64 && dest < 50_i64
        packet_queues[dest.to_i].push({x, y})
      end
    end
  end

  # check idle condition
  idle = true
  50.times do |i|
    if !packet_queues[i].empty? || !computers[i].idle
      idle = false
      break
    end
  end

  if idle && nat_packet
    x, y = nat_packet.not_nil!
    packet_queues[0].push({x, y})
    if prev_nat_y && y == prev_nat_y
      puts y
      exit
    end
    prev_nat_y = y
  end
end
