
registers = Hash.new(0)
mul_count = 0

instructions = File.readlines('input.txt').map(&:split)

pc = 0
while pc >= 0 && pc < instructions.size
  ins, x, y = instructions[pc]
  y_val = y.match?(/-?\d+/) ? y.to_i : registers[y]

  case ins
  when 'set'
    registers[x] = y_val
  when 'sub'
    registers[x] -= y_val
  when 'mul'
    registers[x] *= y_val
    mul_count += 1
  when 'jnz'
    x_val = x.match?(/-?\d+/) ? x.to_i : registers[x]
    if x_val != 0
      pc += y_val - 1
    end
  end

  pc += 1
end

puts mul_count
