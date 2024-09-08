class IntcodeComputer
  def initialize(program, address)
    @memory = program.dup
    @ip = 0
    @relative_base = 0
    @inputs = [address]
    @outputs = []
  end

  def run
    loop do
      opcode = @memory[@ip] % 100
      modes = [@memory[@ip] / 100 % 10, @memory[@ip] / 1000 % 10, @memory[@ip] / 10000 % 10]

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
      when 99 then return :halt
      else raise "Unknown opcode: #{opcode}"
      end

      return :output if @outputs.size == 3
      return :need_input if opcode == 3 && @inputs.empty?
    end
  end

  def add_input(value)
    @inputs << value
  end

  def get_outputs
    outputs = @outputs
    @outputs = []
    outputs
  end

  private

  def get_param(mode, offset)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] || 0
    when 1 then @memory[@ip + offset] || 0
    when 2 then @memory[(@memory[@ip + offset] || 0) + @relative_base] || 0
    end
  end

  def set_param(mode, offset, value)
    case mode
    when 0 then @memory[@memory[@ip + offset] || 0] = value
    when 2 then @memory[(@memory[@ip + offset] || 0) + @relative_base] = value
    end
  end

  def add(modes)
    set_param(modes[2], 3, get_param(modes[0], 1) + get_param(modes[1], 2))
    @ip += 4
  end

  def multiply(modes)
    set_param(modes[2], 3, get_param(modes[0], 1) * get_param(modes[1], 2))
    @ip += 4
  end

  def input(modes)
    set_param(modes[0], 1, @inputs.shift)
    @ip += 2
  end

  def output(modes)
    @outputs << get_param(modes[0], 1)
    @ip += 2
  end

  def jump_if_true(modes)
    @ip = get_param(modes[0], 1) != 0 ? get_param(modes[1], 2) : @ip + 3
  end

  def jump_if_false(modes)
    @ip = get_param(modes[0], 1) == 0 ? get_param(modes[1], 2) : @ip + 3
  end

  def less_than(modes)
    set_param(modes[2], 3, get_param(modes[0], 1) < get_param(modes[1], 2) ? 1 : 0)
    @ip += 4
  end

  def equals(modes)
    set_param(modes[2], 3, get_param(modes[0], 1) == get_param(modes[1], 2) ? 1 : 0)
    @ip += 4
  end

  def adjust_relative_base(modes)
    @relative_base += get_param(modes[0], 1)
    @ip += 2
  end
end

program = File.read('input.txt').strip.split(',').map(&:to_i)
computers = 50.times.map { |i| IntcodeComputer.new(program, i) }
packets = Hash.new { |h, k| h[k] = [] }

loop do
  computers.each_with_index do |computer, address|
    status = computer.run

    if status == :need_input
      input = packets[address].empty? ? -1 : packets[address].shift
      computer.add_input(input)
    elsif status == :output
      dest, x, y = computer.get_outputs
      if dest == 255
        puts y
        exit
      else
        packets[dest] << x << y
      end
    end
  end
end
