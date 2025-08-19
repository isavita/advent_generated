
#!/usr/bin/env crystal
require "json"

class VM
  getter output : Array(Int64)

  def initialize(base_code : Hash(Int64, Int64))
    @code = base_code.dup
    @ip = 0_i64
    @relative_base = 0_i64
    @input = [] of Int64
    @output = [] of Int64
  end

  def input=(values : Array(Int64))
    @input = values.dup
  end

  def run
    loop do
      cmd = @code.fetch(@ip, 0_i64)
      opcode = (cmd % 100).to_i
      modes = Array.new(3) { |i| (cmd // (10_i64 ** (i + 2))) % 10 }

      get_param = ->(idx : Int32) do
        mode = modes[idx - 1]
        val = @code.fetch(@ip + idx, 0_i64)
        case mode
        when 0 then @code.fetch(val, 0_i64)                     # position
        when 1 then val                                         # immediate
        when 2 then @code.fetch(@relative_base + val, 0_i64)    # relative
        else    0_i64
        end
      end

      get_addr = ->(idx : Int32) do
        mode = modes[idx - 1]
        val = @code.fetch(@ip + idx, 0_i64)
        case mode
        when 0 then val
        when 2 then @relative_base + val
        else    0_i64
        end
      end

      case opcode
      when 1
        @code[get_addr.call(3)] = get_param.call(1) + get_param.call(2)
        @ip += 4
      when 2
        @code[get_addr.call(3)] = get_param.call(1) * get_param.call(2)
        @ip += 4
      when 3
        @code[get_addr.call(1)] = @input.shift
        @ip += 2
      when 4
        @output << get_param.call(1)
        @ip += 2
      when 5
        @ip = get_param.call(1) != 0 ? get_param.call(2) : @ip + 3
      when 6
        @ip = get_param.call(1) == 0 ? get_param.call(2) : @ip + 3
      when 7
        @code[get_addr.call(3)] = (get_param.call(1) < get_param.call(2)) ? 1_i64 : 0_i64
        @ip += 4
      when 8
        @code[get_addr.call(3)] = (get_param.call(1) == get_param.call(2)) ? 1_i64 : 0_i64
        @ip += 4
      when 9
        @relative_base += get_param.call(1)
        @ip += 2
      when 99
        break
      else
        raise "unknown opcode #{opcode}"
      end
    end
  end
end

def beam?(x : Int64, y : Int64, base_code : Hash(Int64, Int64)) : Bool
  vm = VM.new(base_code)
  vm.input = [x, y]
  vm.run
  vm.output.first == 1_i64
end

# ---------- main ----------
code_str = File.read("input.txt").strip
base_code = {} of Int64 => Int64
code_str.split(',').each_with_index do |s, i|
  base_code[i.to_i64] = s.to_i64
end

sum = 0_i64
0.upto(49) do |y|
  0.upto(49) do |x|
    sum += 1 if beam?(x.to_i64, y.to_i64, base_code)
  end
end

puts sum
