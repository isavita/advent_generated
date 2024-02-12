
file = File.open("input.txt")
ip_bind = 0
instructions = [] of Array(String)
registers = [0, 0, 0, 0, 0, 0]

ip_bind = file.gets.to_s.match(/\d+/).to_s.to_i

file.each_line do |line|
  instructions << line.split
end

loop do
  ip = registers[ip_bind]
  break if ip < 0 || ip >= instructions.size

  inst = instructions[ip]
  opcode, a, b, c = inst[0], inst[1].to_i, inst[2].to_i, inst[3].to_i

  case opcode
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

  registers[ip_bind] += 1
end

puts registers[0]
