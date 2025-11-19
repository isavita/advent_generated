
class VM
  getter code : Hash(Int64, Int64)
  getter ip : Int64
  getter input : Array(Int64)
  getter output : Array(Int64)
  getter relative_base : Int64
  getter input_index : Int32

  def initialize(filename : String)
    @code = Hash(Int64, Int64).new(0_i64)
    @ip = 0_i64
    @input = [] of Int64
    @output = [] of Int64
    @relative_base = 0_i64
    @input_index = 0
    load(filename)
  end

  def load(filename : String)
    File.read(filename).split(',').each_with_index do |s, i|
      @code[i.to_i64] = s.to_i64
    end
  end

  def run
    while true
      cmd = @code[@ip]
      opcode = cmd % 100

      case opcode
      when 1
        arity = 3
        op_add(cmd, arity)
      when 2
        arity = 3
        op_multiply(cmd, arity)
      when 3
        arity = 1
        op_read(cmd, arity)
      when 4
        arity = 1
        op_write(cmd, arity)
      when 5
        arity = 2
        op_jump_not_zero(cmd, arity)
        next
      when 6
        arity = 2
        op_jump_zero(cmd, arity)
        next
      when 7
        arity = 3
        op_less_than(cmd, arity)
      when 8
        arity = 3
        op_equal(cmd, arity)
      when 9
        arity = 1
        op_change_relative_base(cmd, arity)
      when 99
        return
      else
        raise "not an opcode #{cmd}"
      end

      @ip += arity + 1
    end
  end

  private def op_add(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    @code[params[2]] = @code[params[0]] + @code[params[1]]
  end

  private def op_multiply(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    @code[params[2]] = @code[params[0]] * @code[params[1]]
  end

  private def op_read(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    if @input_index >= @input.size
      raise "Not enough input"
    end
    @code[params[0]] = @input[@input_index]
    @input_index += 1
  end

  private def op_write(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    @output << @code[params[0]]
  end

  private def op_jump_not_zero(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    if @code[params[0]] != 0
      @ip = @code[params[1]]
    else
      @ip += arity + 1
    end
  end

  private def op_jump_zero(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    if @code[params[0]] == 0
      @ip = @code[params[1]]
    else
      @ip += arity + 1
    end
  end

  private def op_less_than(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    @code[params[2]] = @code[params[0]] < @code[params[1]] ? 1_i64 : 0_i64
  end

  private def op_equal(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    @code[params[2]] = @code[params[0]] == @code[params[1]] ? 1_i64 : 0_i64
  end

  private def op_change_relative_base(cmd, arity)
    params = get_params_addresses(@ip, cmd, arity)
    @relative_base += @code[params[0]]
  end

  private def get_params_addresses(pos, cmd, arity)
    modes = get_modes(cmd, arity)
    results = [] of Int64
    (0...arity).each do |i|
      results << get_param_address(pos + i + 1, modes[i])
    end
    results
  end

  private def get_param_address(pos, mode)
    case mode
    when 0
      @code[pos]
    when 1
      pos
    when 2
      @relative_base + @code[pos]
    else
      raise "wrong mode"
    end
  end

  private def get_modes(cmd, arity)
    mode_section = cmd // 100
    modes = [] of Int64
    (0...arity).each do |i|
      modes << (mode_section // (10_i64 ** i)) % 10
    end
    modes
  end
end

def send_string(input, s)
  s.each_byte do |b|
    input << b.to_i64
  end
  input << '\n'.ord.to_i64
end

vm = VM.new("input.txt")

instructions = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
  "NOT A T",
  "AND A T",
  "OR E T",
  "OR H T",
  "AND T J",
  "RUN",
]

instructions.each do |i|
  send_string(vm.input, i)
end

vm.run

puts vm.output[-1]
