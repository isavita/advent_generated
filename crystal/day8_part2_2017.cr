
file = File.open("input.txt")
registers = Hash(String, Int32).new
highest_value = 0

file.each_line do |line|
  parts = line.split
  reg = parts[0]
  op = parts[1]
  amount = parts[2].to_i
  cond_reg = parts[4]
  cond_op = parts[5]
  cond_val = parts[6].to_i

  registers[reg] ||= 0
  registers[cond_reg] ||= 0

  cond = case cond_op
         when ">"
           registers[cond_reg] > cond_val
         when ">="
           registers[cond_reg] >= cond_val
         when "<"
           registers[cond_reg] < cond_val
         when "<="
           registers[cond_reg] <= cond_val
         when "=="
           registers[cond_reg] == cond_val
         when "!="
           registers[cond_reg] != cond_val
         else
           false
         end

  if cond
    case op
    when "inc"
      registers[reg] += amount
    when "dec"
      registers[reg] -= amount
    end

    highest_value = registers[reg] if registers[reg] > highest_value
  end
end

puts highest_value
