
class OP
  property a : Char
  property b : Char
  property action : Char
  property name : String
  property match_count : Array(UInt8)

  def initialize(@name, @action, @a, @b)
    @match_count = [] of UInt8
  end
end

def run_op(op : OP, registers : Array(Int32), instruction : Array(UInt8)) : Array(Int32)
  register_cp = registers.dup
  a_val = op.a == 'r' ? register_cp[instruction[1]] : instruction[1].to_i32
  b_val = op.b == 'r' ? register_cp[instruction[2]] : instruction[2].to_i32
  case op.action
  when '+'
    register_cp[instruction[3]] = a_val + b_val
  when '*'
    register_cp[instruction[3]] = a_val * b_val
  when '&'
    register_cp[instruction[3]] = a_val & b_val
  when '|'
    register_cp[instruction[3]] = a_val | b_val
  when 'a'
    register_cp[instruction[3]] = a_val
  when '>'
    register_cp[instruction[3]] = a_val > b_val ? 1 : 0
  when '='
    register_cp[instruction[3]] = a_val == b_val ? 1 : 0
  end
  register_cp
end

def test_code(registers, result, instruction, opcodes)
  sum = 0
  opcodes.each do |op|
    if run_op(op, registers, instruction) == result
      op.match_count << instruction[0] unless op.match_count.includes?(instruction[0])
      sum += 1
    end
  end
  sum
end

lines = File.read_lines("input.txt")
opcodes = [
  OP.new("addr", '+', 'r', 'r'),
  OP.new("addi", '+', 'r', 'v'),
  OP.new("mulr", '*', 'r', 'r'),
  OP.new("muli", '*', 'r', 'v'),
  OP.new("banr", '&', 'r', 'r'),
  OP.new("bani", '&', 'r', 'v'),
  OP.new("borr", '|', 'r', 'r'),
  OP.new("bori", '|', 'r', 'v'),
  OP.new("setr", 'a', 'r', 'r'),
  OP.new("seti", 'a', 'v', 'r'),
  OP.new("gtir", '>', 'v', 'r'),
  OP.new("gtri", '>', 'r', 'v'),
  OP.new("gtrr", '>', 'r', 'r'),
  OP.new("eqir", '=', 'v', 'r'),
  OP.new("eqri", '=', 'r', 'v'),
  OP.new("eqrr", '=', 'r', 'r'),
]

line_count = 0
while line_count < lines.size && lines[line_count].starts_with?('B')
  registers = lines[line_count][9..-1].gsub(/\[|\]/, "").split(",").map(&.strip.to_i32)
  instruction = lines[line_count + 1].split.map(&.to_u8)
  result = lines[line_count + 2][9..-1].gsub(/\[|\]/, "").split(",").map(&.strip.to_i32)
  test_code(registers, result, instruction, opcodes)
  line_count += 4
end

ordered_op_codes = {} of UInt8 => OP
while ordered_op_codes.size < 16
  opcodes.each do |op|
    if op.match_count.size == 1
      c = op.match_count[0]
      ordered_op_codes[c] = op
      opcodes.each { |o| o.match_count.delete(c) }
    end
  end
end

line_count += 2
r = [0_i32, 0_i32, 0_i32, 0_i32]
lines[line_count..].each do |line|
  instruction = line.split.map(&.to_u8)
  r = run_op(ordered_op_codes[instruction[0]], r, instruction)
end

puts r[0]
