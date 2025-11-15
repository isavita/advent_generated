
struct Program
  getter a : Int64
  getter b : Int64
  getter c : Int64
  getter program : Array(Int32)

  def initialize(@a : Int64, @b : Int64, @c : Int64, @program : Array(Int32))
  end
end

def compute_operand(val : Int32, a : Int64, b : Int64, c : Int64) : Int64
  case val
  when 0..3 then val.to_i64
  when 4    then a
  when 5    then b
  when 6    then c
  else           raise "Invalid combo operand: #{val}"
  end
end

def simulate_computer(program : Program) : Array(Int32)
  a, b, c = program.a, program.b, program.c
  input = program.program
  outs = [] of Int32

  i = 0
  while i < input.size
    cmd = input[i]
    case cmd
    when 0
      a >>= compute_operand(input[i + 1], a, b, c)
    when 1
      b ^= input[i + 1].to_i64
    when 2
      b = compute_operand(input[i + 1], a, b, c) & 7
    when 3
      if a != 0
        i = input[i + 1] - 2
      end
    when 4
      b ^= c
    when 5
      outs << (compute_operand(input[i + 1], a, b, c) & 7).to_i32
    when 6
      b = a >> compute_operand(input[i + 1], a, b, c)
    when 7
      c = a >> compute_operand(input[i + 1], a, b, c)
    else
      raise "Invalid opcode: #{cmd}"
    end
    i += 2
  end
  outs
end

def check(p : Program) : Array(Int64)
  valids = [] of Int64
  stack = [{0, 0_i64}]
  seen = Set({Int32, Int64}).new

  until stack.empty?
    depth, score = stack.pop
    next if seen.includes?({depth, score})
    seen.add({depth, score})

    if depth == p.program.size
      valids << score
    else
      8.times do |i|
        new_score = i.to_i64 + 8_i64 * score
        test = Program.new(new_score, p.b, p.c, p.program)
        result = simulate_computer(test)
        if !result.empty? && result[0] == p.program[p.program.size - 1 - depth]
          stack << {depth + 1, new_score}
        end
      end
    end
  end
  valids
end

lines = File.read_lines("input.txt")
a = lines[0].split(": ")[1].to_i64
b = lines[1].split(": ")[1].to_i64
c = lines[2].split(": ")[1].to_i64
program = lines[4].split(": ")[1].split(",").map(&.to_i32)

p = Program.new(a, b, c, program)
valids = check(p)
puts valids.empty? ? "No valid values found" : valids.min
