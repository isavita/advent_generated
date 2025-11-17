
#!/usr/bin/env crystal
def main
  instructions = File.read_lines("input.txt").reject(&.empty?)
  a = 1_u64
  b = 0_u64
  pc = 0

  while pc < instructions.size
    instr = instructions[pc]
    opcode = instr[0,3]
    case opcode
    when "hlf"
      case instr[4]
      when 'a' then a //= 2
      when 'b' then b //= 2
      end
      pc += 1
    when "tpl"
      case instr[4]
      when 'a' then a *= 3_u64
      when 'b' then b *= 3_u64
      end
      pc += 1
    when "inc"
      case instr[4]
      when 'a' then a += 1_u64
      when 'b' then b += 1_u64
      end
      pc += 1
    when "jmp"
      offset = instr[4..].to_i
      pc += offset
    when "jie"
      reg = instr[4]
      offset = instr[7..].to_i
      val = reg == 'a' ? a : b
      pc += (val & 1 == 0) ? offset : 1
    when "jio"
      reg = instr[4]
      offset = instr[7..].to_i
      val = reg == 'a' ? a : b
      pc += (val == 1_u64) ? offset : 1
    else
      raise "unknown instruction #{instr}"
    end
  end

  puts b
end

main
