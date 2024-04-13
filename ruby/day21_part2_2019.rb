class VM
  attr_accessor :code, :ip, :input, :output, :relative_base

  def initialize(filename)
    @code = {}
    @ip = 0
    @relative_base = 0
    @input = Queue.new
    @output = Queue.new
    load(filename)
  end

  def load(filename)
    File.read(filename).strip.split(",").each_with_index do |num, i|
      @code[i] = num.to_i
    end
  end

  def run
    loop do
      cmd = Cmd.new(@code[@ip])
      case cmd.op_code
      when 1, 2, 7, 8
        params = get_params_addresses(@ip, cmd, 3)
        @code[params[2]] = case cmd.op_code
                           when 1
                             @code[params[0]] + @code[params[1]]
                           when 2
                             @code[params[0]] * @code[params[1]]
                           when 7
                             (@code[params[0]] < @code[params[1]]) ? 1 : 0
                           when 8
                             (@code[params[0]] == @code[params[1]]) ? 1 : 0
                           end
      when 3
        params = get_params_addresses(@ip, cmd, 1)
        @code[params[0]] = @input.pop
      when 4
        params = get_params_addresses(@ip, cmd, 1)
        @output.push(@code[params[0]])
      when 5, 6
        params = get_params_addresses(@ip, cmd, 2)
        if (cmd.op_code == 5 && @code[params[0]] != 0) || (cmd.op_code == 6 && @code[params[0]] == 0)
          @ip = @code[params[1]]
          next
        end
      when 9
        params = get_params_addresses(@ip, cmd, 1)
        @relative_base += @code[params[0]]
      when 99
        return
      else
        raise "Unknown opcode #{cmd.op_code}"
      end
      @ip += cmd.arity + 1
    end
  end

  def get_params_addresses(pos, cmd, arity)
    modes = cmd.modes(arity)
    (1..arity).map { |i| get_param_address(pos + i, modes[i - 1]) }
  end

  def get_param_address(pos, mode)
    case mode
    when 0
      @code[pos] || 0
    when 1
      pos
    when 2
      @relative_base + (@code[pos] || 0)
    else
      raise "Wrong mode #{mode}"
    end
  end
end

class Cmd
  def initialize(value)
    @value = value
  end

  def op_code
    @value % 100
  end

  def modes(arity)
    modes = (@value / 100).digits
    Array.new(arity) { |i| modes[i] || 0 }
  end

  def arity
    case op_code
    when 1, 2, 7, 8
      3
    when 3, 4, 9
      1
    when 5, 6
      2
    when 99
      0
    else
      raise "Unknown opcode #{op_code}"
    end
  end
end

def send_string(queue, string)
  string.each_char { |char| queue.push(char.ord) }
  queue.push(10) # Newline as '\n'
end

vm = VM.new("input.txt")
instructions = [
  "NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J",
  "NOT A T", "AND A T", "OR E T", "OR H T", "AND T J", "RUN"
]
instructions.each { |instruction| send_string(vm.input, instruction) }
vm.run
done = false
while !done
  begin
    output = vm.output.pop(true)
    if output > 127
      puts output
      done = true
    end
  rescue ThreadError
    done = true
  end
end