
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

a = 0
b = 0
c = 0
program = []

File.foreach("input.txt") do |line|
  line = line.strip
  next if line.empty?

  if line.start_with?("Register A:")
    a = line.split(":").last.strip.to_i
  elsif line.start_with?("Register B:")
    b = line.split(":").last.strip.to_i
  elsif line.start_with?("Register C:")
    c = line.split(":").last.strip.to_i
  elsif line.start_with?("Program:")
    program = line.split(":").last.strip.split(",").map(&:to_i)
  end
end

output_vals = []
ip = 0

while ip < program.length
  opcode = program[ip]
  break if ip + 1 >= program.length
  operand = program[ip + 1]
  
  case opcode
  when 0 # adv
    den = get_combo_val(operand, a, b, c)
    a = den == 0 ? 0 : a / (2**den)
    ip += 2
  when 1 # bxl
    b ^= operand
    ip += 2
  when 2 # bst
    b = get_combo_val(operand, a, b, c) % 8
    ip += 2
  when 3 # jnz
    ip = a != 0 ? operand : ip + 2
  when 4 # bxc
    b ^= c
    ip += 2
  when 5 # out
    output_vals << (get_combo_val(operand, a, b, c) % 8).to_s
    ip += 2
  when 6 # bdv
      den = get_combo_val(operand, a, b, c)
      b = a / (2**den)
      ip += 2
    when 7 # cdv
      den = get_combo_val(operand, a, b, c)
      c = a / (2**den)
      ip += 2
  else
      break
  end
end

puts output_vals.join(",")
