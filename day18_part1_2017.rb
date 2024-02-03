
def get_value(registers, val)
  if val.to_i.to_s == val
    val.to_i
  else
    registers[val]
  end
end

registers = Hash.new(0)
instructions = File.readlines("input.txt").map(&:split)
last_sound = 0
i = 0

while i >= 0 && i < instructions.length
  ins = instructions[i]
  case ins[0]
  when "snd"
    last_sound = get_value(registers, ins[1])
  when "set"
    registers[ins[1]] = get_value(registers, ins[2])
  when "add"
    registers[ins[1]] += get_value(registers, ins[2])
  when "mul"
    registers[ins[1]] *= get_value(registers, ins[2])
  when "mod"
    registers[ins[1]] %= get_value(registers, ins[2])
  when "rcv"
    if get_value(registers, ins[1]) != 0
      puts last_sound
      break
    end
  when "jgz"
    i += get_value(registers, ins[2]) - 1 if get_value(registers, ins[1]) > 0
  end
  i += 1
end
