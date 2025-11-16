
class VM
  getter code : Hash(Int64, Int64)
  getter ip : Int64
  getter input : Deque(Int64)
  getter output : Deque(Int64)
  getter relative_base : Int64

  def initialize(filename)
    @ip = 0_i64
    @relative_base = 0_i64
    @code = Hash(Int64, Int64).new(0_i64)
    File.read(filename).chomp.split(",").each_with_index do |seg, idx|
      @code[idx.to_i64] = seg.to_i64
    end
    @input = Deque(Int64).new
    @output = Deque(Int64).new
  end

  def get_value(addr : Int64)
    @code[addr]
  end

  def set_value(addr : Int64, val : Int64)
    @code[addr] = val
  end

  def get_param(index : Int32, modes : Array(Int32)) : Int64
    mode = modes[index - 1]
    imm = get_value(@ip + index)
    case mode
    when 0 then get_value(imm)
    when 1 then imm
    when 2 then get_value(@relative_base + imm)
    else 0_i64
    end
  end

  def get_address(index : Int32, modes : Array(Int32)) : Int64
    mode = modes[index - 1]
    imm = get_value(@ip + index)
    case mode
    when 0 then imm
    when 2 then @relative_base + imm
    else 0_i64
    end
  end

  def run
    while true
      instr = get_value(@ip)
      opcode = instr % 100
      modes = [
        ((instr // 100) % 10).to_i,
        ((instr // 1000) % 10).to_i,
        ((instr // 10000) % 10).to_i
      ]
      case opcode
      when 1
        val1 = get_param(1, modes)
        val2 = get_param(2, modes)
        addr = get_address(3, modes)
        set_value(addr, val1 + val2)
        @ip += 4
      when 2
        val1 = get_param(1, modes)
        val2 = get_param(2, modes)
        addr = get_address(3, modes)
        set_value(addr, val1 * val2)
        @ip += 4
      when 3
        addr = get_address(1, modes)
        set_value(addr, @input.shift)
        @ip += 2
      when 4
        @output << get_param(1, modes)
        @ip += 2
      when 5
        if get_param(1, modes) != 0
          @ip = get_param(2, modes)
        else
          @ip += 3
        end
      when 6
        if get_param(1, modes) == 0
          @ip = get_param(2, modes)
        else
          @ip += 3
        end
      when 7
        val1 = get_param(1, modes)
        val2 = get_param(2, modes)
        addr = get_address(3, modes)
        set_value(addr, val1 < val2 ? 1_i64 : 0_i64)
        @ip += 4
      when 8
        val1 = get_param(1, modes)
        val2 = get_param(2, modes)
        addr = get_address(3, modes)
        set_value(addr, val1 == val2 ? 1_i64 : 0_i64)
        @ip += 4
      when 9
        @relative_base += get_param(1, modes)
        @ip += 2
      when 99
        break
      else
        raise "Unknown opcode #{opcode} at ip #{@ip}"
      end
    end
  end
end

def send_string(vm, s)
  s.each_byte { |c| vm.input << c.to_i64 }
  vm.input << 10_i64
end

vm = VM.new("input.txt")
["NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "WALK"].each do |instr|
  send_string(vm, instr)
end
vm.run
puts vm.output.find { |o| o > 127 }.not_nil!
