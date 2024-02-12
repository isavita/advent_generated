
file = File.open("input.txt")
registers = Hash(String, Int32).new

while line = file.gets
  parts = line.split
  reg = parts[0]
  op = parts[1]
  amount = parts[2].to_i
  condReg = parts[4]
  condOp = parts[5]
  condVal = parts[6].to_i

  registers[reg] ||= 0
  registers[condReg] ||= 0

  cond = case condOp
    when ">"  then registers[condReg] > condVal
    when ">=" then registers[condReg] >= condVal
    when "<"  then registers[condReg] < condVal
    when "<=" then registers[condReg] <= condVal
    when "==" then registers[condReg] == condVal
    when "!=" then registers[condReg] != condVal
  end

  case cond
    when true
      case op
        when "inc" then registers[reg] += amount
        when "dec" then registers[reg] -= amount
      end
  end
end

max_value = registers.values.max
puts max_value
