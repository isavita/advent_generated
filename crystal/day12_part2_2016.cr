file = File.new("input.txt", "r")
instructions = file.gets_to_end.split("\n")

registers = {"a" => 0, "b" => 0, "c" => 1, "d" => 0}
execute_instructions(instructions, registers)
puts registers["a"]

def execute_instructions(instructions, registers)
  i = 0
  while i < instructions.size
    parts = instructions[i].split
    case parts[0]
    when "cpy"
      val = get_value(parts[1], registers)
      registers[parts[2].delete(":")] = val
      i += 1
    when "inc"
      registers[parts[1].delete(":")] += 1
      i += 1
    when "dec"
      registers[parts[1].delete(":")] -= 1
      i += 1
    when "jnz"
      val = get_value(parts[1], registers)
      if val != 0
        jump = parts[2].to_i
        i += jump
      else
        i += 1
      end
    end
  end
end

def get_value(s, registers)
  if s.to_i? != nil
    s.to_i
  else
    registers[s.delete(":")]
  end
end