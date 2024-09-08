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
      modes = [100, 1000, 10000].map { |i| @memory[@ip] / i % 10 }

      case opcode
      when 1 then op_add(modes)
      when 2 then op_multiply(modes)
      when 3 then op_input(modes)
      when 4 then op_output(modes)
      when 5 then op_jump_if_true(modes)
      when 6 then op_jump_if_false(modes)
      when 7 then op_less_than(modes)
      when 8 then op_equals(modes)
      when 9 then op_adjust_relative_base(modes)
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

  def read(mode, pos)
    case mode
    when 0 then @memory[@memory[pos] || 0] || 0
    when 1 then @memory[pos] || 0
    when 2 then @memory[(@memory[pos] || 0) + @relative_base] || 0
    end
  end

  def write(mode, pos, value)
    case mode
    when 0 then @memory[@memory[pos] || 0] = value
    when 2 then @memory[(@memory[pos] || 0) + @relative_base] = value
    end
  end

  def op_add(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) + read(modes[1], @ip + 2))
    @ip += 4
  end

  def op_multiply(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) * read(modes[1], @ip + 2))
    @ip += 4
  end

  def op_input(modes)
    value = @inputs.empty? ? -1 : @inputs.shift
    write(modes[0], @ip + 1, value)
    @ip += 2
  end

  def op_output(modes)
    @outputs << read(modes[0], @ip + 1)
    @ip += 2
  end

  def op_jump_if_true(modes)
    @ip = read(modes[0], @ip + 1) != 0 ? read(modes[1], @ip + 2) : @ip + 3
  end

  def op_jump_if_false(modes)
    @ip = read(modes[0], @ip + 1) == 0 ? read(modes[1], @ip + 2) : @ip + 3
  end

  def op_less_than(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) < read(modes[1], @ip + 2) ? 1 : 0)
    @ip += 4
  end

  def op_equals(modes)
    write(modes[2], @ip + 3, read(modes[0], @ip + 1) == read(modes[1], @ip + 2) ? 1 : 0)
    @ip += 4
  end

  def op_adjust_relative_base(modes)
    @relative_base += read(modes[0], @ip + 1)
    @ip += 2
  end
end

class Network
  def initialize(program)
    @computers = 50.times.map { IntcodeComputer.new(program) }
    @queues = Array.new(50) { [] }
    @nat = nil
    @last_nat_y = nil
  end

  def run
    @computers.each_with_index { |c, i| c.add_input(i) }

    part1_answer = nil
    part2_answer = nil

    loop do
      idle = true

      @computers.each_with_index do |computer, address|
        computer.run

        while (dest = computer.get_output)
          idle = false
          x = computer.get_output
          y = computer.get_output

          if dest == 255
            @nat = [x, y]
            part1_answer ||= y
          else
            @queues[dest] << [x, y]
          end
        end

        if @queues[address].empty?
          computer.add_input(-1)
        else
          idle = false
          x, y = @queues[address].shift
          computer.add_input(x)
          computer.add_input(y)
        end
      end

      if idle && @nat
        x, y = @nat
        @computers[0].add_input(x)
        @computers[0].add_input(y)

        if y == @last_nat_y
          part2_answer = y
          break
        end

        @last_nat_y = y
      end

      break if part1_answer && part2_answer
    end

    puts "Part 1: #{part1_answer}"
    puts "Part 2: #{part2_answer}"
  end
end

program = File.read('input.txt').strip
Network.new(program).run
