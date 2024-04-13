def load_program(lines)
  program = []
  ip_register = nil
  lines.each do |line|
    if line.start_with?("#ip")
      ip_register = line.split.last.to_i
    else
      parts = line.split
      op, a, b, c = parts[0], parts[1].to_i, parts[2].to_i, parts[3].to_i
      program << [op, a, b, c]
    end
  end
  [ip_register, program]
end

def run_program(ip_register, program, registers, max_cycles)
  ip = 0
  cycles = 0
  while ip >= 0 && ip < program.length
    registers[ip_register] = ip
    op, a, b, c = program[ip]
    case op
    when "addr"; registers[c] = registers[a] + registers[b]
    when "addi"; registers[c] = registers[a] + b
    when "mulr"; registers[c] = registers[a] * registers[b]
    when "muli"; registers[c] = registers[a] * b
    when "banr"; registers[c] = registers[a] & registers[b]
    when "bani"; registers[c] = registers[a] & b
    when "borr"; registers[c] = registers[a] | registers[b]
    when "bori"; registers[c] = registers[a] | b
    when "setr"; registers[c] = registers[a]
    when "seti"; registers[c] = a
    when "gtir"; registers[c] = a > registers[b] ? 1 : 0
    when "gtri"; registers[c] = registers[a] > b ? 1 : 0
    when "gtrr"; registers[c] = registers[a] > registers[b] ? 1 : 0
    when "eqir"; registers[c] = a == registers[b] ? 1 : 0
    when "eqri"; registers[c] = registers[a] == b ? 1 : 0
    when "eqrr"; registers[c] = registers[a] == registers[b] ? 1 : 0
    end
    ip = registers[ip_register] + 1
    cycles += 1
    break if max_cycles > 0 && cycles >= max_cycles
  end
  registers
end

def max_value(array)
  array.max
end

lines = File.readlines("input.txt").map(&:chomp).reject(&:empty?)
ip_register, program = load_program(lines)
registers = Array.new(6, 0)
registers[0] = 1
registers = run_program(ip_register, program, registers, 1000)
n = max_value(registers)
total = 0
(1..n).each do |i|
  total += i if n % i == 0
end
puts total