
file = File.read("input.txt")
instructions = file.split("\n")

def execute_instructions(instructions, registers)
  i = 0
  while i < instructions.size
    parts = instructions[i].split
    case parts[0]
    when "cpy"
      val = get_value(parts[1], registers)
      registers[parts[2]] = val
      i += 1
    when "inc"
      registers[parts[1]] += 1
      i += 1
    when "dec"
      registers[parts[1]] -= 1
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
  begin
    return s.to_i
  rescue
    return registers[s]
  end
end

registers = {"a" => 0, "b" => 0, "c" => 0, "d" => 0}
execute_instructions(instructions, registers)

puts registers["a"]
