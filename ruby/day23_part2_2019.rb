
class VM
  attr_accessor :code, :ip, :input, :output, :relative_base

  def initialize(filename, chan_size)
    @code = {}
    load(filename)
    @ip = 0
    @input = Queue.new
    @output = Queue.new
    @relative_base = 0
  end

  def load(filename)
    File.read(filename).strip.split(',').each_with_index do |str, i|
      @code[i] = str.to_i
    end
  end

  def run
    loop do
      cmd = Cmd.new(@code[@ip])
      case cmd.opcode
      when 1
        arity = 3
        params = get_params_addresses(@ip, cmd, arity)
        @code[params[2]] = @code[params[0]] + @code[params[1]]
      when 2
        arity = 3
        params = get_params_addresses(@ip, cmd, arity)
        @code[params[2]] = @code[params[0]] * @code[params[1]]
      when 3
        arity = 1
        params = get_params_addresses(@ip, cmd, arity)
        @code[params[0]] = @input.pop
      when 4
        arity = 1
        params = get_params_addresses(@ip, cmd, arity)
        @output.push(@code[params[0]])
      when 5
        arity = 2
        params = get_params_addresses(@ip, cmd, arity)
        if @code[params[0]] != 0
          @ip = @code[params[1]]
          next
        end
      when 6
        arity = 2
        params = get_params_addresses(@ip, cmd, arity)
        if @code[params[0]] == 0
          @ip = @code[params[1]]
          next
        end
      when 7
        arity = 3
        params = get_params_addresses(@ip, cmd, arity)
        @code[params[2]] = @code[params[0]] < @code[params[1]] ? 1 : 0
      when 8
        arity = 3
        params = get_params_addresses(@ip, cmd, arity)
        @code[params[2]] = @code[params[0]] == @code[params[1]] ? 1 : 0
      when 9
        arity = 1
        params = get_params_addresses(@ip, cmd, arity)
        @relative_base += @code[params[0]]
      when 99
        return
      else
        raise "Unknown opcode: #{cmd.opcode}"
      end
      @ip += arity + 1
    end
  end

  def get_params_addresses(pos, cmd, arity)
    modes = cmd.modes(arity)
    results = []
    arity.times do |i|
      results << get_param_address(pos + i + 1, modes[i])
    end
    results
  end

  def get_param_address(pos, mode)
    case mode
    when 0
      @code[pos]
    when 1
      pos
    when 2
      @relative_base + @code[pos]
    else
      raise "Invalid mode #{mode}"
    end
  end
end

class Cmd
  attr_reader :instruction
  def initialize(instruction)
    @instruction = instruction
  end

  def opcode
    @instruction % 100
  end

  def modes(arity)
    mode_section = @instruction / 100
    modes = []
    arity.times do |i|
      modes << (mode_section / (10**i) % 10)
    end
    modes
  end
end

SIZE = 50

def main
  vms = Array.new(SIZE)
  nat_ch = Queue.new

  SIZE.times do |i|
    vms[i] = VM.new('input.txt', 10000)
    Thread.new { vms[i].run }
    vms[i].input.push(i)
    vms[i].input.push(-1)
  end

  SIZE.times do |i|
    Thread.new { router(vms, i, nat_ch) }
  end

  puts nat(vms, nat_ch)
end

def router(vms, id, nat_ch)
    loop do
      address = vms[id].output.pop
      x = vms[id].output.pop
      y = vms[id].output.pop
    
      if address == 255
        nat_ch.push(x)
        nat_ch.push(y)
      else
        vms[address].input.push(x)
        vms[address].input.push(y)
      end
    end
end


def nat(vms, nat_ch)
  x = -1
  y = -1

  Thread.new do
    loop do
      x = nat_ch.pop
      y = nat_ch.pop
    end
  end

  last_y = -1
  loop do
    sleep(0.01)
      if x != -1 && idle(vms)
        vms[0].input.push(x)
        vms[0].input.push(y)
          if y == last_y
          return y
        end
      last_y = y
    end
  end
end

def idle(vms)
    vms.all? { |vm| vm.input.empty? }
end

main
