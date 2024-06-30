class IntcodeComputer
  def initialize(program)
    @memory = program.split(',').map(&:to_i)
    @ip = 0
    @relative_base = 0
    @inputs = []
    @outputs = []
  end

  def run
    loop do
      opcode = @memory[@ip] % 100
      modes = 3.times.map { |i| @memory[@ip] / (10 ** (i + 2)) % 10 }

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
      when 99 then return :halted
      else raise "Unknown opcode: #{opcode}"
      end

      return :output if @outputs.size == 3
      return :need_input if opcode == 3 && @inputs.empty?
    end
  end

  def add_input(value)
    @inputs << value
  end

  def get_output
    @outputs.shift(3)
  end

  private

  def get_param(offset, mode)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] || 0
    when 1 then @memory[@ip + offset] || 0
    when 2 then @memory[(@relative_base + @memory[@ip + offset]) || 0] || 0
    end
  end

  def set_param(offset, mode, value)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] = value
    when 2 then @memory[(@relative_base + @memory[@ip + offset]) || 0] = value
    end
  end

  def add(modes)
    set_param(3, modes[2], get_param(1, modes[0]) + get_param(2, modes[1]))
    @ip += 4
  end

  def multiply(modes)
    set_param(3, modes[2], get_param(1, modes[0]) * get_param(2, modes[1]))
    @ip += 4
  end

  def input(modes)
    return if @inputs.empty?
    set_param(1, modes[0], @inputs.shift)
    @ip += 2
  end

  def output(modes)
    @outputs << get_param(1, modes[0])
    @ip += 2
  end

  def jump_if_true(modes)
    @ip = get_param(1, modes[0]) != 0 ? get_param(2, modes[1]) : @ip + 3
  end

  def jump_if_false(modes)
    @ip = get_param(1, modes[0]) == 0 ? get_param(2, modes[1]) : @ip + 3
  end

  def less_than(modes)
    set_param(3, modes[2], get_param(1, modes[0]) < get_param(2, modes[1]) ? 1 : 0)
    @ip += 4
  end

  def equals(modes)
    set_param(3, modes[2], get_param(1, modes[0]) == get_param(2, modes[1]) ? 1 : 0)
    @ip += 4
  end

  def adjust_relative_base(modes)
    @relative_base += get_param(1, modes[0])
    @ip += 2
  end
end

class Network
  def initialize(program)
    @computers = 50.times.map { IntcodeComputer.new(program) }
    @packet_queues = Array.new(50) { [] }
  end

  def run
    # Boot up and assign network addresses
    @computers.each_with_index do |computer, address|
      computer.add_input(address)
    end

    loop do
      @computers.each_with_index do |computer, address|
        status = computer.run

        if status == :need_input
          input = @packet_queues[address].empty? ? -1 : @packet_queues[address].shift
          computer.add_input(input)
        elsif status == :output
          dest, x, y = computer.get_output
          return y if dest == 255
          @packet_queues[dest] << x << y if dest < 50
        end
      end
    end
  end
end

# Read input
program = File.read('input.txt').strip

# Create and run the network
network = Network.new(program)
result = network.run

puts "Y value of the first packet sent to address 255: #{result}"
