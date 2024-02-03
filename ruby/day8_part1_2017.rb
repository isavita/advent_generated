
registers = Hash.new(0)

File.open("input.txt").each do |line|
  reg, op, val, _, cond_reg, cond_op, cond_val = line.split
  val, cond_val = val.to_i, cond_val.to_i

  if registers[cond_reg].public_send(cond_op, cond_val)
    registers[reg] += op == 'inc' ? val : -val
  end
end

puts registers.values.max
