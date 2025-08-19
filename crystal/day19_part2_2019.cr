
class Cmd
  def initialize(@value : Int64); end

  def opcode : Int64
    @value % 100
  end

  def modes(arity) : Array(Int64)
    m = @value // 100
    Array.new(arity) { v = m % 10; m //= 10; v }
  end
end

class VM
  def initialize(filename : String)
    @code = {} of Int64 => Int64
    @ip = 0_i64
    @relative_base = 0_i64
    @input = [] of Int64
    @output = [] of Int64
    load(filename)
  end

  def load(filename)
    data = File.read(filename).strip.split(',').map(&.to_i64)
    data.each_with_index { |v, i| @code[i.to_i64] = v }
  end

  def input=(arr : Array(Int64))
    @input = arr.dup
  end

  def output : Array(Int64)
    @output
  end

  def run
    loop do
      cmd = Cmd.new(@code.fetch(@ip, 0_i64))
      opcode = cmd.opcode
      case opcode
      when 1, 2, 7, 8
        arity = 3_i64
        params = get_params_addresses(@ip, cmd, arity)
        a = @code.fetch(params[0], 0_i64)
        b = @code.fetch(params[1], 0_i64)
        case opcode
        when 1 then @code[params[2]] = a + b
        when 2 then @code[params[2]] = a * b
        when 7 then @code[params[2]] = a < b ? 1_i64 : 0_i64
        when 8 then @code[params[2]] = a == b ? 1_i64 : 0_i64
        end
      when 3
        arity = 1_i64
        params = get_params_addresses(@ip, cmd, arity)
        @code[params[0]] = @input.shift
      when 4
        arity = 1_i64
        params = get_params_addresses(@ip, cmd, arity)
        @output << @code.fetch(params[0], 0_i64)
      when 5, 6
        arity = 2_i64
        params = get_params_addresses(@ip, cmd, arity)
        cond = @code.fetch(params[0], 0_i64) != 0_i64
        cond = !cond if opcode == 6
        if cond
          @ip = @code.fetch(params[1], 0_i64)
          next
        end
      when 9
        arity = 1_i64
        params = get_params_addresses(@ip, cmd, arity)
        @relative_base += @code.fetch(params[0], 0_i64)
      when 99
        return
      else
        raise "Invalid opcode #{opcode}"
      end
      @ip += arity + 1
    end
  end

  private def get_params_addresses(pos, cmd, arity)
    modes = cmd.modes(arity)
    Array.new(arity) { |i| get_param_address(pos + i + 1, modes[i]) }
  end

  private def get_param_address(pos, mode)
    case mode
    when 0 then @code.fetch(pos, 0_i64)
    when 1 then pos
    when 2 then @relative_base + @code.fetch(pos, 0_i64)
    else raise "Bad mode #{mode}"
    end
  end
end

def beam(x : Int64, y : Int64) : Bool
  vm = VM.new("input.txt")
  vm.input = [x, y]
  vm.run
  vm.output.last == 1_i64
end

def main
  y = 20_i64
  x = 0_i64
  loop do
    unless beam(x, y)
      x += 1
      next
    end
    unless beam(x + 99, y)
      y += 1
      next
    end
    unless beam(x, y + 99)
      x += 1
      next
    end
    puts x * 10000 + y
    break
  end
end

main
