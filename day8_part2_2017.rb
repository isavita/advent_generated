registers = Hash.new(0)
max_value = 0

File.open("input.txt").each do |line|
  parts = line.split
  reg = parts[0]
  op = parts[1]
  val = parts[2].to_i
  cond_reg = parts[4]
  cond_op = parts[5]
  cond_val = parts[6].to_i

  if eval("registers[cond_reg] #{cond_op} #{cond_val}")
    if op == "inc"
      registers[reg] += val
    else
      registers[reg] -= val
    end
    max_value = [max_value, registers.values.max].max
  end
end

puts registers.values.max
puts max_value