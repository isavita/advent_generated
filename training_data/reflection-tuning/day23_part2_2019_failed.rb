class Computer
  attr_reader :address, :queue, :waiting_for_input

  def initialize(address, program)
    @address = address
    @program = program.dup
    @queue = []
    @waiting_for_input = false
    @pc = 0
    @relative_base = 0
  end

  def run
    loop do
      opcode = @program[@pc] % 100
      modes = [@program[@pc] / 100 % 10, @program[@pc] / 1000 % 10, @program[@pc] / 10000 % 10]

      case opcode
      when 1 # add
        set_value(3, get_value(1, modes[0]) + get_value(2, modes[1]), modes[2])
        @pc += 4
      when 2 # multiply
        set_value(3, get_value(1, modes[0]) * get_value(2, modes[1]), modes[2])
        @pc += 4
      when 3 # input
        if @queue.empty?
          @waiting_for_input = true
          return -1
        end
        set_value(1, @queue.shift, modes[0])
        @pc += 2
      when 4 # output
        @pc += 2
        return get_value(1, modes[0])
      when 5 # jump-if-true
        @pc = get_value(1, modes[0]) != 0 ? get_value(2, modes[1]) : @pc + 3
      when 6 # jump-if-false
        @pc = get_value(1, modes[0]) == 0 ? get_value(2, modes[1]) : @pc + 3
      when 7 # less than
        set_value(3, get_value(1, modes[0]) < get_value(2, modes[1]) ? 1 : 0, modes[2])
        @pc += 4
      when 8 # equals
        set_value(3, get_value(1, modes[0]) == get_value(2, modes[1]) ? 1 : 0, modes[2])
        @pc += 4
      when 9 # adjust relative base
        @relative_base += get_value(1, modes[0])
        @pc += 2
      when 99 # halt
        return nil
      end
    end
  end

  def get_value(param, mode)
    case mode
    when 0 then @program[@program[@pc + param]] || 0
    when 1 then @program[@pc + param] || 0
    when 2 then @program[@relative_base + (@program[@pc + param] || 0)] || 0
    end
  end

  def set_value(param, value, mode)
    case mode
    when 0 then @program[@program[@pc + param]] = value
    when 2 then @program[@relative_base + @program[@pc + param]] = value
    end
  end
end

def solve(program)
  computers = 50.times.map { |i| Computer.new(i, program) }
  computers.each { |c| c.queue << c.address }

  nat_packet = nil
  last_nat_y = nil

  loop do
    idle = true

    computers.each do |computer|
      3.times do
        output = computer.run
        if output.nil?
          break
        elsif output != -1
          idle = false
          dest, x, y = output, computer.run, computer.run
          if dest == 255
            nat_packet = [x, y]
          else
            computers[dest].queue.concat([x, y])
          end
        end
      end
    end

    if idle && nat_packet
      computers[0].queue.concat(nat_packet)
      return nat_packet[1] if nat_packet[1] == last_nat_y
      last_nat_y = nat_packet[1]
      idle = false
    end
  end
end

# Assuming the Intcode program is provided as a string of comma-separated integers
program = ARGF.read.strip.split(',').map(&:to_i)
puts solve(program)
