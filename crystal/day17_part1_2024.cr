
def get_combo_val(op, a, b, c)
  case op
  when 0..3
    op
  when 4
    a
  when 5
    b
  when 6
    c
  else
    raise "invalid combo operand"
  end
end

lines = File.read("input.txt").split("\n")
a = 0
b = 0
c = 0
program = [] of Int32

lines.each do |line|
  s = line.strip
  next if s.empty?
  if s.starts_with?("Register A:")
    a = s.split(":")[1].strip.to_i
  elsif s.starts_with?("Register B:")
    b = s.split(":")[1].strip.to_i
  elsif s.starts_with?("Register C:")
    c = s.split(":")[1].strip.to_i
  elsif s.starts_with?("Program:")
    s.split(":")[1].strip.split(",").each do |n|
      program << n.strip.to_i
    end
  end
end

output_vals = [] of String
ip = 0
while ip < program.size
  opcode = program[ip]
  break if ip + 1 >= program.size
  operand = program[ip + 1]

  case opcode
  when 0
    den = get_combo_val(operand, a, b, c)
    a = den == 0 ? 0 : a // (1 << den)
    ip += 2
  when 1
    b ^= operand
    ip += 2
  when 2
    b = get_combo_val(operand, a, b, c) % 8
    ip += 2
  when 3
    ip = a != 0 ? operand : ip + 2
  when 4
    b ^= c
    ip += 2
  when 5
    output_vals << (get_combo_val(operand, a, b, c) % 8).to_s
    ip += 2
  when 6
    b = a // (1 << get_combo_val(operand, a, b, c))
    ip += 2
  when 7
    c = a // (1 << get_combo_val(operand, a, b, c))
    ip += 2
  else
    break
  end
end

puts output_vals.join(",")
