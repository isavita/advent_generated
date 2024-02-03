
def execute_instruction(instruction, registers)
  op, a, b, c = instruction
  case op
  when "addr"
    registers[c] = registers[a] + registers[b]
  when "addi"
    registers[c] = registers[a] + b
  when "mulr"
    registers[c] = registers[a] * registers[b]
  when "muli"
    registers[c] = registers[a] * b
  when "banr"
    registers[c] = registers[a] & registers[b]
  when "bani"
    registers[c] = registers[a] & b
  when "borr"
    registers[c] = registers[a] | registers[b]
  when "bori"
    registers[c] = registers[a] | b
  when "setr"
    registers[c] = registers[a]
  when "seti"
    registers[c] = a
  when "gtir"
    registers[c] = a > registers[b] ? 1 : 0
  when "gtri"
    registers[c] = registers[a] > b ? 1 : 0
  when "gtrr"
    registers[c] = registers[a] > registers[b] ? 1 : 0
  when "eqir"
    registers[c] = a == registers[b] ? 1 : 0
  when "eqri"
    registers[c] = registers[a] == b ? 1 : 0
  when "eqrr"
    registers[c] = registers[a] == registers[b] ? 1 : 0
  end
end

file = File.open("input.txt")
lines = file.readlines.map(&:chomp)
file.close

ip_bind = lines.shift.match(/#ip (\d)/)[1].to_i
instructions = lines.map { |line| op, a, b, c = line.split; [op, a.to_i, b.to_i, c.to_i] }

registers = [0, 0, 0, 0, 0, 0]
ip = 0

while ip >= 0 && ip < instructions.length
  registers[ip_bind] = ip
  execute_instruction(instructions[ip], registers)
  ip = registers[ip_bind]
  ip += 1
end

puts registers[0]
