
require "json"

enum Mode : Int32
  POSITION = 0
  IMMEDIATE = 1
  RELATIVE = 2
end

enum Opcode : Int32
  ADD = 1
  MUL = 2
  INPUT = 3
  OUTPUT = 4
  JT = 5
  JF = 6
  LT = 7
  EQ = 8
  RBO = 9
  HALT = 99
end

def decode(v : Int64) : {Int32, Array(Int32)}
  op = (v % 100).to_i
  v //= 100
  {op, [ (v % 10).to_i, ((v // 10) % 10).to_i, (v // 100).to_i ]}
end

class Machine
  @data : Hash(Int64, Int64)
  @ip = 0_i64
  @rel = 0_i64
  @out = [] of Int64

  def initialize(program : Array(Int64))
    @data = Hash(Int64, Int64).new(0_i64)
    program.each_with_index { |v, i| @data[i.to_i64] = v }
  end

  def get(idx : Int64, mode : Int32) : Int64
    case mode
    when Mode::IMMEDIATE.value then @data[idx]
    when Mode::POSITION.value  then @data[@data[idx]]
    when Mode::RELATIVE.value then @data[@rel + @data[idx]]
    else raise "mode"
    end
  end

  def set(idx : Int64, mode : Int32, val : Int64)
    case mode
    when Mode::POSITION.value then @data[@data[idx]] = val
    when Mode::RELATIVE.value then @data[@rel + @data[idx]] = val
    else raise "mode"
    end
  end

  def step : Bool
    op, modes = decode(@data[@ip])
    case op
    when Opcode::ADD.value
      set(@ip + 3, modes[2], get(@ip + 1, modes[0]) + get(@ip + 2, modes[1])); @ip += 4
    when Opcode::MUL.value
      set(@ip + 3, modes[2], get(@ip + 1, modes[0]) * get(@ip + 2, modes[1])); @ip += 4
    when Opcode::INPUT.value
      # no input needed
      @ip += 2
    when Opcode::OUTPUT.value
      @out << get(@ip + 1, modes[0]); @ip += 2
    when Opcode::JT.value
      @ip = get(@ip + 1, modes[0]) != 0 ? get(@ip + 2, modes[1]) : @ip + 3
    when Opcode::JF.value
      @ip = get(@ip + 1, modes[0]) == 0 ? get(@ip + 2, modes[1]) : @ip + 3
    when Opcode::LT.value
      set(@ip + 3, modes[2], get(@ip + 1, modes[0]) < get(@ip + 2, modes[1]) ? 1_i64 : 0_i64); @ip += 4
    when Opcode::EQ.value
      set(@ip + 3, modes[2], get(@ip + 1, modes[0]) == get(@ip + 2, modes[1]) ? 1_i64 : 0_i64); @ip += 4
    when Opcode::RBO.value
      @rel += get(@ip + 1, modes[0]); @ip += 2
    when Opcode::HALT.value
      return false
    else
      raise "opcode"
    end
    true
  end

  def run : Array(Int64)
    while step; end
    @out
  end
end

def count_blocks(program : Array(Int64)) : Int32
  out = Machine.new(program).run
  cnt = 0_i32
  i = 0
  while i < out.size
    t = out[i + 2]
    cnt += 1 if t == 2_i64
    i += 3
  end
  cnt
end

def main
  prog = File.read("input.txt").strip.split(',').map(&.to_i64)
  puts count_blocks(prog)
end

main
