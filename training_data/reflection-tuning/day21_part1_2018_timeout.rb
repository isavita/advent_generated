def execute(op, a, b, c, registers)
  case op
  when 'addr' then registers[c] = registers[a] + registers[b]
  when 'addi' then registers[c] = registers[a] + b
  when 'mulr' then registers[c] = registers[a] * registers[b]
  when 'muli' then registers[c] = registers[a] * b
  when 'banr' then registers[c] = registers[a] & registers[b]
  when 'bani' then registers[c] = registers[a] & b
  when 'borr' then registers[c] = registers[a] | registers[b]
  when 'bori' then registers[c] = registers[a] | b
  when 'setr' then registers[c] = registers[a]
  when 'seti' then registers[c] = a
  when 'gtir' then registers[c] = a > registers[b] ? 1 : 0
  when 'gtri' then registers[c] = registers[a] > b ? 1 : 0
  when 'gtrr' then registers[c] = registers[a] > registers[b] ? 1 : 0
  when 'eqir' then registers[c] = a == registers[b] ? 1 : 0
  when 'eqri' then registers[c] = registers[a] == b ? 1 : 0
  when 'eqrr' then registers[c] = registers[a] == registers[b] ? 1 : 0
  end
end

input = File.readlines('input.txt', chomp: true)
ip_reg = input.shift.split.last.to_i
instructions = input.map { |line| line.split.map { |x| x =~ /\d+/ ? x.to_i : x } }

def run_program(instructions, ip_reg, initial_r0)
  registers = [initial_r0, 0, 0, 0, 0, 0]
  ip = 0
  executed_instructions = 0

  while ip >= 0 && ip < instructions.length
    registers[ip_reg] = ip
    op, a, b, c = instructions[ip]
    execute(op, a, b, c, registers)
    ip = registers[ip_reg]
    ip += 1
    executed_instructions += 1

    # Early exit condition (adjust based on program analysis)
    return nil if executed_instructions > 1000000
  end

  executed_instructions
end

r0 = 0
loop do
  result = run_program(instructions, ip_reg, r0)
  if result
    puts r0
    break
  end
  r0 += 1
end
