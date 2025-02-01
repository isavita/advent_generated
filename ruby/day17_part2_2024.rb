
def compute_operand(val, a, b, c)
  case val
  when 0, 1, 2, 3 then val
  when 4 then a
  when 5 then b
  when 6 then c
  else raise "Invalid combo operand: #{val}"
  end
end

def simulate_computer(program)
  outs = []
  a, b, c = program[:a], program[:b], program[:c]
  input = program[:program]

  i = 1
  while i <= input.length
    cmd = input[i - 1]
    case cmd
    when 0
      a >>= compute_operand(input[i], a, b, c)
    when 1
      b ^= input[i]
    when 2
      b = compute_operand(input[i], a, b, c) % 8
    when 3
      i = input[i] - 1 if a != 0
    when 4
      b ^= c
    when 5
      outs << compute_operand(input[i], a, b, c) % 8
    when 6
      b = a >> compute_operand(input[i], a, b, c)
    when 7
      c = a >> compute_operand(input[i], a, b, c)
    else raise "Invalid opcode: #{cmd}"
    end
    i += 2
  end
  outs
end

def check(p)
  program = p[:program]
  valids = []
  stack = [[0, 0]]
  seen = {}

  until stack.empty?
    depth, score = stack.pop

    next if seen[[depth, score]]
    seen[[depth, score]] = true

    if depth == program.length
      valids << score
    else
      (0...8).each do |i|
        new_score = i + 8 * score
        test_program = { a: new_score, b: p[:b], c: p[:c], program: program }
        result = simulate_computer(test_program)
        if !result.empty? && result[0] == program[program.length - 1 - depth]
          stack << [depth + 1, new_score]
        end
      end
    end
  end
  valids
end

a, b, c = 0, 0, 0
program = []

File.foreach("input.txt") do |line|
  line.strip!
  if line.start_with?("Register A:")
    a = line.split(":")[1].strip.to_i
  elsif line.start_with?("Register B:")
    b = line.split(":")[1].strip.to_i
  elsif line.start_with?("Register C:")
    c = line.split(":")[1].strip.to_i
  elsif line.start_with?("Program:")
    program = line.split(":")[1].strip.split(",").map(&:to_i)
  end
end

p = { a: a, b: b, c: c, program: program }

valid_values = check(p)
min_val = valid_values.min

puts min_val
