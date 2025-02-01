
class VM
  attr_accessor :code, :ip, :input, :output, :relative_base

  def initialize(filename)
    @input = []
    @output = []
    @ip = 0
    @relative_base = 0
    load(filename)
  end

  def load(filename)
    list_str = File.read(filename).strip.split(',')
    @code = Hash.new(0)
    list_str.each_with_index { |val, i| @code[i] = val.to_i }
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
        @code[params[0]] = @input.shift
      when 4
        arity = 1
        params = get_params_addresses(@ip, cmd, arity)
        @output << @code[params[0]]
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
        raise "not an opcode #{cmd}"
      end
      @ip += arity + 1
    end
  end

  def get_params_addresses(pos, cmd, arity)
    modes = cmd.modes(arity)
    results = []
    arity.times { |i| results << get_param_address(pos + i + 1, modes[i]) }
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
      raise 'wrong mode'
    end
  end
end

class Cmd
  attr_reader :value

  def initialize(value)
    @value = value
  end

  def opcode
    @value % 100
  end

  def modes(arity)
    mode_section = @value / 100
    modes = []
    arity.times { |i| modes << mode_section / (10**i) % 10 }
    modes
  end
end

def beam(x, y)
  vm = VM.new('input.txt')
  vm.input << x << y
  vm.run
  vm.output.shift == 1
end

y = 20
x = 0

loop do
  if !beam(x, y)
    x += 1
    next
  end

  if !beam(x + 99, y)
    y += 1
    next
  end

  if !beam(x, y + 99)
    x += 1
    next
  end

  puts x * 10000 + y
  break
end
