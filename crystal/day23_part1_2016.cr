
def read_instructions(filename : String) : Array(String)
  File.read(filename).lines
end

def execute_instructions(instructions : Array(String), registers : Hash(String, Int32))
  pc = 0
  while pc < instructions.size
    fields = instructions[pc].split(" ")
    case fields[0]
    when "cpy"
      x = fields[1].to_i? ? fields[1].to_i : registers[fields[1]]
      registers[fields[2]] = x if registers.has_key?(fields[2])
    when "inc"
      registers[fields[1]] += 1 if registers.has_key?(fields[1])
    when "dec"
      registers[fields[1]] -= 1 if registers.has_key?(fields[1])
    when "jnz"
      x = fields[1].to_i? ? fields[1].to_i : registers[fields[1]]
      if x != 0
        pc += (fields[2].to_i? ? fields[2].to_i : registers[fields[2]]) - 1
      end
    when "tgl"
      x = fields[1].to_i? ? fields[1].to_i : registers[fields[1]]
      tgt = pc + x
      if tgt >= 0 && tgt < instructions.size
        instructions[tgt] = toggle_instruction(instructions[tgt])
      end
    end
    pc += 1
  end
end

def toggle_instruction(instr : String) : String
  parts = instr.split(" ")
  case parts[0]
  when "inc"
    parts[0] = "dec"
  when "dec", "tgl"
    parts[0] = "inc"
  when "jnz"
    parts[0] = "cpy"
  when "cpy"
    parts[0] = "jnz"
  end
  parts.join(" ")
end

instructions = read_instructions("input.txt")
registers = {"a" => 7, "b" => 0, "c" => 0, "d" => 0}
execute_instructions(instructions, registers)
puts registers["a"]
